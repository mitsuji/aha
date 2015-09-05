{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String(fromString)
import System.Environment (getArgs)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)
import WaiAppStatic.Types (toPieces)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Parse as Parse
import qualified Network.WebSockets as WS
import Control.Concurrent (ThreadId,forkIO,threadDelay,MVar,newMVar,readMVar,modifyMVar,modifyMVar_)
import Data.Typeable(Typeable)
import Control.Exception (Exception,catch,throwIO,throwTo,finally)
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as BS -- use for input 
import qualified Data.ByteString.Lazy.Char8 as LBS -- use for output
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID

import Data.Aeson.Types(ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE
import System.IO(withFile,IOMode(ReadMode,WriteMode))

import System.Posix.Signals(installHandler,sigINT,sigTERM,Handler(Catch))

import qualified Control.Concurrent.STM as STM

import Control.Concurrent.Async(race)

import Control.Applicative ((<$>),(<*>))

import Data.Data(Data,toConstr)


type ReporterKey = String

data Reporter = Reporter
  { reporterKey  :: ReporterKey
  , reporterAha  :: STM.TVar Int
  , reporterChan :: STM.TChan Message
  }

newReporter :: ReporterKey -> STM.STM Reporter
newReporter rk = do
  aha   <- STM.newTVar 0
  chan  <- STM.newBroadcastTChan 
  return Reporter { reporterKey  = rk
                  , reporterAha  = aha
                  , reporterChan = chan
                  }


type BoardSecretKey = String
type BoardPublicKey = String
type BoardCaption   = String

data Board = Board
  { boardSecretKey :: BoardSecretKey
  , boardPublicKey :: BoardPublicKey
  , boardCaption   :: BoardCaption
  , boardAha       :: STM.TVar Int
  , boardReporters :: STM.TVar (Map.Map ReporterKey Reporter)
  , boardChan      :: STM.TChan Message
  }

newBoard :: BoardSecretKey -> BoardPublicKey -> BoardCaption -> STM.STM Board
newBoard bsk bpk caption = do
  aha       <- STM.newTVar 0
  reporters <- STM.newTVar Map.empty
  chan      <- STM.newBroadcastTChan 
  return Board { boardSecretKey = bsk
               , boardPublicKey = bpk
               , boardCaption   = caption
               , boardAha       = aha
               , boardReporters = reporters
               , boardChan      = chan
               }

  
data Server = Server
  { serverBoards    :: STM.TVar (Map.Map BoardPublicKey Board)
  , serverBoardKeys :: STM.TVar (Map.Map BoardSecretKey BoardPublicKey)
  }

newServer :: IO Server
newServer = do
  boards <- STM.newTVarIO Map.empty
  keys   <- STM.newTVarIO Map.empty
  return Server { serverBoards = boards, serverBoardKeys = keys }
  
  


data Response = ResponseBoard BoardSecretKey BoardPublicKey BoardCaption
              | ResponseReset
              deriving (Show)

instance ToJSON Response where
  toJSON (ResponseBoard sk pk ca) =
    object ["success" .= True
           ,"type"    .= ("board" :: String)
           ,"content" .= object ["secret_key" .= sk
                                ,"public_key" .= pk
                                ,"caption"    .= ca
                                ]
           ]
  toJSON (ResponseReset) =
    object ["success" .= True
           ,"type"    .= ("reset" :: String)
           ,"content" .= ("ok" :: String)
           ]



data Message = MessageBoard BoardPublicKey BoardCaption
             | MessageReporter ReporterKey BoardPublicKey BoardCaption
             | MessageAha Int
             | MessageTotalAha Int
             | MessageReset
             deriving (Show)
                      
instance ToJSON Message where
  toJSON (MessageBoard pk ca) =
    object ["type"    .= ("board" :: String)
           ,"content" .= object ["public_key" .= pk
                                ,"caption"    .= ca
                                ]
           ]
  toJSON (MessageReporter rk bpk ca) =
    object ["type"    .= ("reporter" :: String)
           ,"content" .= object ["reporter_key"     .= rk
                                ,"board_public_key" .= bpk
                                ,"board_caption"    .= ca
                                ]
           ]
  toJSON (MessageAha aha) =
    object ["type"    .= ("aha" :: String)
           ,"content" .= aha
           ]
  toJSON (MessageTotalAha ta) =
    object ["type"    .= ("total_aha" :: String)
           ,"content" .= ta
           ]
  toJSON (MessageReset) =
    object ["type" .= ("reset" :: String)]




data Error = BoardSecretKeyInvalid
           | BoardSecretKeyDuplicated
           | BoardSecretKeyNotSpecified
           | BoardPublicKeyInvalid
           | BoardPublicKeyDuplicated
           | BoardPublicKeyNotSpecified
           | BoardCaptionInvalid
           | BoardCaptionNotSpecified
           | BoardFromSecretKeyNotFound
           | BoardFromPublicKeyNotFound
           | ReporterKeyInvalid
           | ReporterKeyDuplicated
           | ReporterKeyNotSpecified       -- unused
           | ReporterFromSecretKeyNotFound -- unused
           deriving(Show,Typeable,Data)


instance ToJSON Error where
  toJSON (e) = simpleErrorJSON e

simpleErrorJSON :: Error -> AE.Value
simpleErrorJSON e = object ["success" .= False
                             ,"type"    .= (show $ toConstr e)
                             ]





addBoardIO :: Server -> BoardPublicKey -> BoardCaption -> IO (Either Error Board)
addBoardIO server bpk caption
  | not $ isValidPublicKey bpk   = return $ Left BoardPublicKeyInvalid
  | not $ isValidCaption caption = return $ Left BoardCaptionInvalid
  | otherwise = do
    mubsk <- nextUUID
    case mubsk of
      Nothing   -> return $ Left BoardSecretKeyInvalid
      Just ubsk -> STM.atomically $
                   addBoard server (UUID.toString ubsk) bpk caption
  where
    isValidPublicKey cand = (all (\c -> elem c ("abcdefghijklmnopqrstuvwxyz0123456789" :: String) ) cand)
                            && (0 < length cand && length cand <= 20)
    isValidCaption cand = 0 < length cand && length cand <= 20




addBoard :: Server -> BoardSecretKey -> BoardPublicKey -> BoardCaption -> STM.STM (Either Error Board)
addBoard Server{..} bsk bpk caption = do

  keys   <- STM.readTVar serverBoardKeys
  boards <- STM.readTVar serverBoards

  case () of
    _ | Map.member bsk keys   -> return $ Left BoardSecretKeyDuplicated
      | Map.member bpk boards -> return $ Left BoardPublicKeyDuplicated
      | otherwise -> do
        board <- newBoard bsk bpk caption
        STM.writeTVar serverBoardKeys $ Map.insert bsk bpk keys
        STM.writeTVar serverBoards    $ Map.insert bpk board boards
        return $ Right board


getBoardFromPublicKey :: Server -> BoardPublicKey -> STM.STM (Maybe Board)
getBoardFromPublicKey Server{..} bpk = 
  (Map.lookup bpk) <$> STM.readTVar serverBoards
  

getBoardFromSecretKey :: Server -> BoardSecretKey -> STM.STM (Maybe Board)
getBoardFromSecretKey server@Server{..} bsk = do
  mbpk <- (Map.lookup bsk) <$> STM.readTVar serverBoardKeys
  case mbpk of
    Nothing  -> return $ Nothing
    Just bpk -> getBoardFromPublicKey server bpk




resetBoard :: Board -> STM.STM ()
resetBoard Board{..} = do
  reporters <- STM.readTVar boardReporters

  -- reset all
  STM.writeTVar boardAha 0
  mapM_ (\r -> STM.writeTVar (reporterAha r) 0) (Map.elems reporters)

  -- reset message
  STM.writeTChan boardChan MessageReset
  



addReporterIO :: Board -> IO (Either Error Reporter)
addReporterIO board = do
  murk <- nextUUID
  case murk of
    Nothing  -> return $ Left ReporterKeyInvalid
    Just urk -> STM.atomically $
                addReporter board (UUID.toString urk)


addReporter :: Board -> ReporterKey -> STM.STM (Either Error Reporter)
addReporter Board{..} rk = do
  reporters <- STM.readTVar boardReporters
  if Map.member rk reporters
    then return $ Left ReporterKeyDuplicated
    else do
      reporter <- newReporter rk
      STM.writeTVar boardReporters $ Map.insert rk reporter reporters
      return $ Right reporter
              

getReporter :: Board -> ReporterKey -> STM.STM (Maybe Reporter)
getReporter Board{..} rk =
  (Map.lookup rk) <$> STM.readTVar boardReporters




aha :: Board -> ReporterKey -> STM.STM ()
aha board rk = do
  mreporter <- getReporter board rk
  case mreporter of
    Nothing -> return ()
    Just reporter -> do
      ahaBoard board
      ahaReporter reporter

ahaReporter :: Reporter -> STM.STM ()
ahaReporter Reporter{..} = do
  aha <- STM.readTVar reporterAha
  let aha' = aha +1
  STM.writeTVar reporterAha aha'
  STM.writeTChan reporterChan $ MessageAha aha'
  
ahaBoard :: Board -> STM.STM ()
ahaBoard Board{..} = do
  aha <- STM.readTVar boardAha
  let aha' = aha +1
  STM.writeTVar boardAha aha'
  STM.writeTChan boardChan $ MessageTotalAha aha'
  











main :: IO ()
main = do
  host:port:_ <- getArgs
  server <- newServer
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (websocketApp server) (plainOldHttpApp server)

  


data AhaException = AhaException Error deriving (Show,Typeable)
instance Exception AhaException

throwErrorIO :: Error -> IO a
throwErrorIO error = throwIO $ AhaException error

throwErrorSTM :: Error -> STM.STM a
throwErrorSTM error = STM.throwSTM $ AhaException error




contentTypeJsonHeader :: H.Header
contentTypeJsonHeader = ("Content-Type","application/json")


plainOldHttpApp :: Server -> Wai.Application
plainOldHttpApp server req respond
  | (["reset_board"]  == path) = (resetBoardProc  server req respond) `catch` onError
  | (["add_board"]    == path) = (addBoardProc    server req respond) `catch` onError
  | (["get_board"]    == path) = (getBoardProc    server req respond) `catch` onError
  | otherwise = staticHttpApp req respond -- static html/js/css files
  where
    path = Wai.pathInfo req

    onError :: AhaException -> IO Wai.ResponseReceived
    onError (AhaException error) =
      -- [TODO] log
      respond $ Wai.responseLBS
      H.status200
      [contentTypeJsonHeader]
      (AE.encode error)
    

staticHttpApp :: Wai.Application
staticHttpApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.embeddedSettings $(embedDir "static") -- embed contents as ByteString
    indices = fromJust $ toPieces ["admin.html"] -- default content


lookupParams :: BS.ByteString -> [(BS.ByteString,BS.ByteString)] -> Maybe String
lookupParams key params = do
  b <- Map.lookup key $ Map.fromList params
  return $ T.unpack $ decodeUtf8 b
  



resetBoardProc :: Server -> Wai.Application
resetBoardProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  secretKey <- case lookupParams "secret_key" params of
    Nothing -> throwErrorIO BoardSecretKeyNotSpecified
    Just sk -> return sk

  STM.atomically $ do
    mboard <- getBoardFromSecretKey server secretKey
    case mboard of
      Nothing -> throwErrorSTM BoardFromSecretKeyNotFound
      Just board -> resetBoard board
  
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseReset )




addBoardProc :: Server -> Wai.Application
addBoardProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  publicKey <- case lookupParams "public_key" params of
    Nothing -> throwErrorIO BoardPublicKeyNotSpecified
    Just pk -> return pk

  caption <- case lookupParams "caption" params of
    Nothing -> throwErrorIO BoardCaptionNotSpecified
    Just ca -> return ca

  eboard <- addBoardIO server publicKey caption
  board <- case eboard of
    Left error  -> throwErrorIO error
    Right board -> return board

      
  putStrLn $ "ServerState.addBoard: secretKey: " ++ (boardSecretKey board) ++ " publicKey: " ++ publicKey ++ " caption: " ++ caption
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    (AE.encode $ ResponseBoard (boardSecretKey board) publicKey caption)



getBoardProc :: Server -> Wai.Application
getBoardProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  secretKey <- case lookupParams "secret_key" params of
    Nothing -> throwErrorIO BoardSecretKeyNotSpecified
    Just sk -> return sk

  mboard <- STM.atomically $ getBoardFromSecretKey server secretKey
  board <- case mboard of
    Nothing -> throwErrorIO BoardFromSecretKeyNotFound
    Just board -> return board

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseBoard secretKey (boardPublicKey board) (boardCaption board) )




websocketApp :: Server -> WS.ServerApp
websocketApp server pconn
  | ("/viewer"   == path) = (viewerServer server pconn)   `catch` onError
  | ("/reporter" == path) = (reporterServer server pconn) `catch` onError
  | otherwise = WS.rejectRequest pconn "endpoint not found"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/='?') requestPath

    onError :: AhaException -> IO()
    onError (AhaException error) = do
      -- [TODO] log
      WS.rejectRequest pconn (BS.pack $ show error)


onCloseError :: WS.Connection -> AhaException -> IO()
onCloseError conn (AhaException error) = do
  -- [TODO] log
  WS.sendClose conn (BS.pack $ show error)


viewerServer :: Server -> WS.ServerApp
viewerServer server pconn = do
  putStrLn $ "viewerServer: " ++ BS.unpack(requestPath) -- debug

  publicKey <- case lookupParams "public_key" query of
    Nothing -> throwErrorIO BoardPublicKeyNotSpecified
    Just pk -> return pk

  mboard <- STM.atomically $ getBoardFromPublicKey server publicKey
  board <- case mboard of
    Nothing -> throwErrorIO BoardFromPublicKeyNotFound
    Just board -> return board


  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30


  aha <- STM.atomically $ STM.readTVar (boardAha board)

  WS.sendTextData conn $ AE.encode $ MessageBoard publicKey (boardCaption board)
  WS.sendTextData conn $ AE.encode $ MessageTotalAha aha

  chan <- STM.atomically $ STM.dupTChan $ boardChan board
  (loop conn chan) `catch` (onCloseError conn)

  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    loop :: WS.Connection -> STM.TChan Message -> IO()
    loop conn chan = do
      msg <- STM.atomically $ STM.readTChan chan
      case msg of
        MessageReset -> WS.sendTextData conn $ AE.encode msg
        MessageTotalAha _ -> WS.sendTextData conn $ AE.encode msg
        otherwise -> return ()
      loop conn chan
      
  


    
  

reporterServer :: Server -> WS.ServerApp
reporterServer server pconn = do
  putStrLn $ "reporterServer: " ++ BS.unpack(requestPath) -- debug

  publicKey <- case lookupParams "board_public_key" query of
    Nothing -> throwErrorIO BoardPublicKeyNotSpecified
    Just bpk -> return bpk


  mboard <- STM.atomically $ getBoardFromPublicKey server publicKey
  board <- case mboard of
    Nothing -> throwErrorIO BoardFromPublicKeyNotFound
    Just board -> return board


  reporter <- case lookupParams "reporter_key" query of
    Nothing -> addReporter' board
    Just (rk) -> do
      mreporter <- STM.atomically $ getReporter board rk
      case mreporter of
        Nothing -> addReporter' board -- [TODO] must be an error ?
        Just reporter -> return reporter


  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  (raha,baha) <- STM.atomically $
                 (,) <$> STM.readTVar (reporterAha reporter) <*> STM.readTVar (boardAha board)


  WS.sendTextData conn $ AE.encode $
       MessageReporter (reporterKey reporter) (boardPublicKey board) (boardCaption board)
  WS.sendTextData conn $ AE.encode $ MessageAha raha
  WS.sendTextData conn $ AE.encode $ MessageTotalAha baha


  (reporterTalk conn board reporter) `catch` (onCloseError conn)
    
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    addReporter' board = do
      mreporter <- addReporterIO board
      case mreporter of
        Left error -> throwErrorIO error
        Right reporter -> return reporter


reporterTalk :: WS.Connection -> Board ->  Reporter -> IO()
reporterTalk conn board@Board{..} Reporter{..} = do
  (rchan, bchan) <- STM.atomically $
                    (,) <$> STM.dupTChan reporterChan <*> STM.dupTChan boardChan
  
  race (server rchan bchan) receive
  return()
  where
    server rchan bchan = race (rserver rchan) (bserver bchan)

    rserver chan = do
      msg <- STM.atomically $ STM.readTChan chan
      case msg of
        MessageAha _ -> WS.sendTextData conn $ AE.encode msg
        otherwise -> return ()
      rserver chan

    bserver chan = do
      msg <- STM.atomically $ STM.readTChan chan
      case msg of
        MessageReset -> WS.sendTextData conn $ AE.encode msg
        MessageTotalAha _ -> WS.sendTextData conn $ AE.encode msg
        otherwise -> return ()
      bserver chan

    receive = do
      msg <- WS.receiveData conn :: IO BS.ByteString
      STM.atomically $ aha board reporterKey
      receive





data ReporterJO = ReporterJO
  { jReporterKey  :: ReporterKey
  , jReporterAha  :: Int
  }

data BoardJO = BoardJO
  { jBoardSecretKey :: BoardSecretKey
  , jBoardPublicKey :: BoardPublicKey
  , jBoardCaption   :: BoardCaption
  , jBoardAha       :: Int
  , jBoardReporters :: Map.Map ReporterKey ReporterJO
  }

data ServerJO = ServerJO
  { jServerBoards    :: Map.Map BoardPublicKey BoardJO
  , jServerBoardKeys :: Map.Map BoardSecretKey BoardPublicKey
  }


reporterToJO :: Reporter -> STM.STM ReporterJO
reporterToJO Reporter{..} = do
  aha <- STM.readTVar reporterAha
  return ReporterJO { jReporterKey = reporterKey
                    , jReporterAha = aha
                    }

reporterFromJO :: ReporterJO -> STM.STM Reporter
reporterFromJO ReporterJO{..} = do
  aha   <- STM.newTVar jReporterAha
  chan  <- STM.newBroadcastTChan 
  return Reporter { reporterKey  = jReporterKey
                  , reporterAha  = aha
                  , reporterChan = chan
                  }
  



boardToJO :: Board -> STM.STM BoardJO
boardToJO Board{..} = do
  aha <- STM.readTVar boardAha
  reporters <- STM.readTVar boardReporters
  reporters' <- reportersToJOs reporters
  return BoardJO { jBoardSecretKey = boardSecretKey
                 , jBoardPublicKey = boardPublicKey
                 , jBoardCaption   = boardCaption
                 , jBoardAha       = aha
                 , jBoardReporters = reporters'
                 }
    where
      reporterToJO' :: (ReporterKey,Reporter) -> STM.STM (ReporterKey,ReporterJO)
      reporterToJO' (k,i) = do
        i' <- reporterToJO i
        return (k,i')

      reportersToJOs :: Map.Map ReporterKey Reporter -> STM.STM (Map.Map ReporterKey ReporterJO)
      reportersToJOs reporters = do
        reporters' <- mapM reporterToJO' $ Map.toList reporters
        return $ Map.fromList reporters'
      

boardFromJO :: BoardJO -> STM.STM Board
boardFromJO BoardJO{..} = do
  aha        <- STM.newTVar jBoardAha
  reporters  <- reportersFromJOs jBoardReporters
  reporters' <- STM.newTVar reporters
  chan       <- STM.newBroadcastTChan 
  return Board { boardSecretKey = jBoardSecretKey
               , boardPublicKey = jBoardPublicKey
               , boardCaption   = jBoardCaption
               , boardAha       = aha
               , boardReporters = reporters'
               , boardChan      = chan
               }

    where
      reporterFromJO' :: (ReporterKey,ReporterJO) -> STM.STM (ReporterKey,Reporter)
      reporterFromJO' (k,i) = do
        i' <- reporterFromJO i
        return (k,i')

      reportersFromJOs :: Map.Map ReporterKey ReporterJO -> STM.STM (Map.Map ReporterKey Reporter)
      reportersFromJOs reporters = do
        reporters' <- mapM reporterFromJO' $ Map.toList reporters
        return $ Map.fromList reporters'



serverToJO :: Server -> IO ServerJO
serverToJO Server{..} = do

  -- SecretKeyとPublicKeyが矛盾しないように
  (keys,boards) <- STM.atomically $
                   (,) <$> STM.readTVar serverBoardKeys <*> STM.readTVar serverBoards

  boards' <- boardsToJOs boards
  return ServerJO { jServerBoards    = boards'
                  , jServerBoardKeys = keys
                  }
    where
      boardToJO' :: (BoardPublicKey,Board) -> IO (BoardPublicKey,BoardJO)
      boardToJO' (k,i) = do
        i' <- STM.atomically $ boardToJO i -- BoardとReporterが矛盾しないように
        return (k,i')

      boardsToJOs :: Map.Map BoardPublicKey Board -> IO (Map.Map BoardPublicKey BoardJO)
      boardsToJOs boards = do
        boards' <- mapM boardToJO' $ Map.toList boards
        return $ Map.fromList boards'


serverFromJO :: ServerJO -> STM.STM Server
serverFromJO ServerJO{..} = do
  boards  <- boardsFromJOs jServerBoards
  boards' <- STM.newTVar boards
  keys    <- STM.newTVar jServerBoardKeys
  return Server { serverBoards = boards'
                , serverBoardKeys = keys
                }
    where
      boardFromJO' :: (BoardPublicKey,BoardJO) -> STM.STM (BoardPublicKey,Board)
      boardFromJO' (k,i) = do
        i' <- boardFromJO i
        return (k,i')

      boardsFromJOs :: Map.Map BoardPublicKey BoardJO -> STM.STM (Map.Map BoardPublicKey Board)
      boardsFromJOs boards = do
        boards' <- mapM boardFromJO' $ Map.toList boards
        return $ Map.fromList boards'
