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


type ReporterKey = String

data Reporter = Reporter
  { reporterKey         :: ReporterKey
  , reporterAha         :: STM.TVar Int
  , reporterChan        :: STM.TChan Message
  }

newReporter :: ReporterKey -> STM.STM Reporter
newReporter rk = do
  aha   <- STM.newTVar 0
  chan  <- STM.newBroadcastTChan 
  return Reporter { reporterKey         = rk
                  , reporterAha         = aha
                  , reporterChan        = chan
                  }


type BoardSecretKey = String
type BoardPublicKey = String
type Caption = String

data Board = Board
  { boardSecretKey     :: BoardSecretKey
  , boardPublicKey     :: BoardPublicKey
  , boardCaption       :: Caption
  , boardAha           :: STM.TVar Int
  , boardReporters     :: STM.TVar (Map.Map ReporterKey Reporter)
  , boardChan          :: STM.TChan Message
  }

newBoard :: BoardSecretKey -> BoardPublicKey -> Caption -> STM.STM Board
newBoard bsk bpk caption = do
  aha       <- STM.newTVar 0
  reporters <- STM.newTVar Map.empty
  chan      <- STM.newBroadcastTChan 
  return Board { boardSecretKey     = bsk
               , boardPublicKey     = bpk
               , boardCaption       = caption
               , boardAha           = aha
               , boardReporters     = reporters
               , boardChan          = chan
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
  
  


data Message = MessageBoard BoardPublicKey Caption
             | MessageReporter ReporterKey BoardPublicKey Caption
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




data Error = BoardCaptionInvalid
           | BoardPublicKeyInvalid
           | BoardSecretKeyInvalid
           | BoardPublicKeyDuplicated
           | BoardSecretKeyDuplicated
           | ReporterKeyInvalid
           | ReporterKeyDuplicated
           deriving(Show)


addBoardIO :: Server -> Caption -> BoardPublicKey -> IO (Either Error Board)
addBoardIO server caption bpk
  | not $ isValidCaption caption = return $ Left BoardCaptionInvalid
  | not $ isValidPublicKey bpk   = return $ Left BoardPublicKeyInvalid
  | otherwise = do
    mubsk <- nextUUID
    case mubsk of
      Nothing   -> return $ Left BoardSecretKeyInvalid
      Just ubsk -> STM.atomically $
                   addBoard server caption (UUID.toString ubsk) bpk
  where
    isValidCaption cand = 0 < length cand && length cand <= 20
    isValidPublicKey cand = (all (\c -> elem c ("abcdefghijklmnopqrstuvwxyz0123456789" :: String) ) cand)
                            && (0 < length cand && length cand <= 20)




addBoard :: Server -> Caption -> BoardSecretKey -> BoardPublicKey -> STM.STM (Either Error Board)
addBoard Server{..} caption bsk bpk = do

  boards <- STM.readTVar serverBoards
  keys   <- STM.readTVar serverBoardKeys

  case () of
    _ | Map.member bpk boards -> return $ Left BoardPublicKeyDuplicated
      | Map.member bsk keys   -> return $ Left BoardSecretKeyDuplicated
      | otherwise -> do
        board <- newBoard bsk bpk caption
        STM.writeTVar serverBoards    $ Map.insert bpk board boards
        STM.writeTVar serverBoardKeys $ Map.insert bsk bpk keys
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
--  host:port:backupPath:_ <- getArgs
  host:port:_ <- getArgs
  server <- newServer
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (websocketApp server) (plainOldHttpApp server)

  


type ErrorCode = Int
type ErrorMessage = String

data AhaException = AhaException ErrorCode ErrorMessage deriving (Show,Typeable)
instance Exception AhaException

throwError :: ErrorCode -> ErrorMessage -> IO a
throwError code msg = throwIO $ AhaException code msg




data Response = ResponseError ErrorCode ErrorMessage
              | ResponseBoard BoardSecretKey BoardPublicKey Caption
              | ResponseReset
              deriving (Show)

instance ToJSON Response where
  toJSON (ResponseError code msg) =
    object ["success"    .= False
           ,"error_code" .= code
           ,"message"    .= msg
           ]
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
    onError e@(AhaException code msg) =
      respond $ Wai.responseLBS
      H.status200
      [contentTypeJsonHeader]
      (AE.encode $ ResponseError code msg)


staticHttpApp :: Wai.Application
staticHttpApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.embeddedSettings $(embedDir "static") -- embed contents as ByteString
    indices = fromJust $ toPieces ["admin.html"] -- default content


resetBoardProc :: Server -> Wai.Application
resetBoardProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  secretKey <- case Map.lookup "secret_key" $ Map.fromList params of
    Nothing -> throwError 10001 "\"secret_key\" is not specified"
    Just sk -> return $ T.unpack $ decodeUtf8 sk

  STM.atomically $ do
    mboard <- getBoardFromSecretKey server secretKey
    case mboard of
      Nothing -> return () -- [TODO] throwError
      Just board -> resetBoard board
  
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseReset )




addBoardProc :: Server -> Wai.Application
addBoardProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  publicKey <- case Map.lookup "public_key" $ Map.fromList params of
    Nothing -> throwError 10002 "\"public_key\" is not specified"
    Just pk -> return $ T.unpack $ decodeUtf8 pk

  caption <- case Map.lookup "caption" $ Map.fromList params of
    Nothing -> throwError 10003 "\"caption\" is not specified"
    Just ca -> return $ T.unpack $ decodeUtf8 ca

  eboard <- addBoardIO server caption publicKey
  board <- case eboard of
    Left err@BoardCaptionInvalid      -> throwError 10004 ("addBoardIO failed: " ++ (show err))
    Left err@BoardSecretKeyDuplicated -> throwError 10005 ("addBoardIO failed: " ++ (show err))
    Left err@BoardPublicKeyDuplicated -> throwError 10006 ("addBoardIO failed: " ++ (show err))
    Left err@BoardSecretKeyInvalid    -> throwError 10007 ("addBoardIO failed: " ++ (show err))
    Left err@BoardPublicKeyInvalid    -> throwError 10008 ("addBoardIO failed: " ++ (show err))
    Right board -> return board

      
  putStrLn $ "ServerState.addBoard: secretKey: " ++ (boardSecretKey board) ++ " publicKey: " ++ publicKey ++ " caption: " ++ caption
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    (AE.encode $ ResponseBoard (boardSecretKey board) publicKey caption)



getBoardProc :: Server -> Wai.Application
getBoardProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  secretKey <- case Map.lookup "secret_key" $ Map.fromList params of
    Nothing -> throwError 10001 "\"secret_key\" is not specified"
    Just sk -> return $ T.unpack $ decodeUtf8 sk

  mboard <- STM.atomically $ getBoardFromSecretKey server secretKey
  board <- case mboard of
    Nothing -> throwError 10002 "\"secret_key\" is not found"
    Just board -> return board

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseBoard secretKey (boardPublicKey board) (boardCaption board) )




websocketApp :: Server -> WS.ServerApp
websocketApp server pconn
  | ("/viewer"   == path) = viewerServer server pconn
  | ("/reporter" == path) = reporterServer server pconn
  | otherwise = WS.rejectRequest pconn "request rejected"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/='?') requestPath





viewerServer :: Server -> WS.ServerApp
viewerServer server pconn = do
  putStrLn $ "viewerServer: " ++ BS.unpack(requestPath) -- debug

  publicKey <- case Map.lookup "public_key" $ Map.fromList query of
    Nothing -> throwError 20001 "\"public_key\" is not specified"
    Just pk -> return $ T.unpack $ decodeUtf8 pk

  mboard <- STM.atomically $ getBoardFromPublicKey server publicKey
  board <- case mboard of
    Nothing -> throwError 20002 "\"secret_key\" is not found"
    Just board -> return board


  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30


  aha <- STM.atomically $ STM.readTVar (boardAha board)

  WS.sendTextData conn $ AE.encode $ MessageBoard publicKey (boardCaption board)
  WS.sendTextData conn $ AE.encode $ MessageTotalAha aha

  chan <- STM.atomically $ STM.dupTChan $ boardChan board
  loop conn chan

  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    loop :: WS.Connection -> STM.TChan Message -> IO()
    loop conn chan = do
      msg <- STM.atomically $ STM.readTChan chan
      case msg of
        MessageReset -> WS.sendTextData conn $ AE.encode msg
        MessageTotalAha _ -> WS.sendTextData conn $ AE.encode msg
        otherwise -> throwError 20003 "error"
      loop conn chan
      
  


    
  

reporterServer :: Server -> WS.ServerApp
reporterServer server pconn = do
  putStrLn $ "reporterServer: " ++ BS.unpack(requestPath) -- debug

  publicKey <- case Map.lookup "board_public_key" $ Map.fromList query of
    Nothing -> throwError 20001 "\"board_public_key\" is not specified"
    Just bpk -> return $ T.unpack $ decodeUtf8 bpk


  mboard <- STM.atomically $ getBoardFromPublicKey server publicKey
  board <- case mboard of
    Nothing -> throwError 20002 "\"board_public_key\" is not found"
    Just board -> return board


  reporter <- case Map.lookup "reporter_key" $ Map.fromList query of
    Nothing -> do
      mreporter <- addReporterIO board
      case mreporter of
        Right r -> return r
    Just (brk) -> do
      let rk = T.unpack $ decodeUtf8 brk
      mreporter <- STM.atomically $ getReporter board rk
      case mreporter of
        Just r -> return r
        Nothing -> do
          mreporter <- addReporterIO board
          case mreporter of
            Right r -> return r

  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30


  raha <- STM.atomically $ STM.readTVar (reporterAha reporter)
  baha <- STM.atomically $ STM.readTVar (boardAha board)

  WS.sendTextData conn $ AE.encode $
       MessageReporter (reporterKey reporter) (boardPublicKey board) (boardCaption board)
  WS.sendTextData conn $ AE.encode $ MessageAha raha
  WS.sendTextData conn $ AE.encode $ MessageTotalAha baha


  reporterTalk conn board reporter
    
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath



reporterTalk :: WS.Connection -> Board ->  Reporter -> IO()
reporterTalk conn board@Board{..} Reporter{..} = do
  rchan <- STM.atomically $ STM.dupTChan reporterChan
  bchan <- STM.atomically $ STM.dupTChan boardChan
  race (server rchan bchan) receive
  return()
  where
    server rchan bchan = race (rserver rchan) (bserver bchan)

    rserver chan = do
      msg <- STM.atomically $ STM.readTChan chan
      case msg of
        MessageAha _ -> WS.sendTextData conn $ AE.encode msg
        otherwise -> throwError 20003 "error"
      rserver chan

    bserver chan = do
      msg <- STM.atomically $ STM.readTChan chan
      case msg of
        MessageReset -> WS.sendTextData conn $ AE.encode msg
        MessageTotalAha _ -> WS.sendTextData conn $ AE.encode msg
        otherwise -> throwError 20004 "error"
      bserver chan

    receive = do
      msg <- WS.receiveData conn :: IO BS.ByteString
      STM.atomically $ aha board reporterKey
      receive

