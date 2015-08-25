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



type ConnKey = Int

-- reporterBroadcastChan
data Reporter = Reporter
  { reporterNextConnKey :: STM.TVar ConnKey
  , reporterConnections :: STM.TVar (Map.Map ConnKey WS.Connection)
  , reporterAha         :: STM.TVar Int
  , reporterChan        :: STM.TChan Message
  }

newReporter :: STM.STM Reporter
newReporter = do
  nck   <- STM.newTVar 0
  conns <- STM.newTVar Map.empty
  aha   <- STM.newTVar 0
  chan  <- STM.newBroadcastTChan 
  return Reporter { reporterNextConnKey = nck
                  , reporterConnections = conns
                  , reporterAha         = aha
                  , reporterChan        = chan
                  }


-- viewerBroadcastChan
data Viewer = Viewer WS.Connection


type Caption = String
type ViewerKey = Int
type ReporterKey = String

data Board = Board
  { boardSecretKey     :: BoardSecretKey
  , boardPublicKey     :: BoardPublicKey
  , boardCaption       :: Caption
  , boardAha           :: STM.TVar Int
  , boardNextViewerKey :: STM.TVar ViewerKey
  , boardViewers       :: STM.TVar (Map.Map ViewerKey Viewer)
  , boardReporters     :: STM.TVar (Map.Map ReporterKey Reporter)
  , boardChan          :: STM.TChan Message
  }

newBoard :: BoardSecretKey -> BoardPublicKey -> Caption -> STM.STM Board
newBoard bsk bpk caption = do
  aha       <- STM.newTVar 0
  nvk       <- STM.newTVar 0
  viewers   <- STM.newTVar Map.empty
  reporters <- STM.newTVar Map.empty
  chan      <- STM.newBroadcastTChan 
  return Board { boardSecretKey     = bsk
               , boardPublicKey     = bpk
               , boardCaption       = caption
               , boardAha           = aha
               , boardNextViewerKey = nvk
               , boardViewers       = viewers
               , boardReporters     = reporters
               , boardChan          = chan
               }
  

type BoardSecretKey = String
type BoardPublicKey = String
  
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
  | not $ isValidCaption bpk   = return $ Left BoardCaptionInvalid
  | not $ isValidPublicKey bpk = return $ Left BoardPublicKeyInvalid
  | otherwise = do
    mubsk <- nextUUID
    case mubsk of
      Nothing -> return $ Left BoardSecretKeyInvalid
      Just ubsk ->
        STM.atomically $ addBoard server caption (UUID.toString ubsk) bpk
  where
    isValidCaption cand = 0 < length cand && length cand <= 20
    isValidPublicKey cand = (all (\c -> elem c "abcdefghijklmnopqrstuvwxyz0123456789") cand)
                            && (0 < length cand && length cand <= 20)


addBoard :: Server -> Caption -> BoardSecretKey -> BoardPublicKey -> STM.STM (Either Error Board)
addBoard Server{..} caption bsk bpk = do
  boards <- STM.readTVar serverBoards
  keys <- STM.readTVar serverBoardKeys
  if Map.member bpk boards
    then return $ Left BoardPublicKeyDuplicated
    else if Map.member bsk keys
         then return $ Left BoardSecretKeyDuplicated
         else do
           board <- newBoard bsk bpk caption
           STM.writeTVar serverBoards    $ Map.insert bpk board boards
           STM.writeTVar serverBoardKeys $ Map.insert bsk bpk keys
           return $ Right board


getBoard :: Server -> BoardSecretKey -> STM.STM (Maybe Board)
getBoard Server{..} bsk = do
  keys <- STM.readTVar serverBoardKeys
  boards <- STM.readTVar serverBoards
  bpk <- return $ Map.lookup bsk keys
  board <- return $ Map.lookup (fromJust bpk) boards
  return $ board


resetBoard :: Board -> STM.STM ()
resetBoard Board{..} = do
  reporters <- STM.readTVar boardReporters
  mapM_ (\r -> STM.writeTVar (reporterAha r) 0) (Map.elems reporters)
  STM.writeTVar boardAha 0
  mapM_ (\r -> STM.writeTChan (reporterChan r) (MessageAha 0)) (Map.elems reporters)
  STM.writeTChan boardChan (MessageTotalAha 0)




addReporterIO :: Board -> IO (Either Error Reporter)
addReporterIO board = do
  murk <- nextUUID
  case murk of
    Nothing -> return $ Left ReporterKeyInvalid
    Just urk ->
      STM.atomically $ addReporter board (UUID.toString urk)


addReporter :: Board -> ReporterKey -> STM.STM (Either Error Reporter)
addReporter Board{..} rk = do
  reporters <- STM.readTVar boardReporters
  if Map.member rk reporters
    then return $ Left ReporterKeyDuplicated
    else do
      reporter <- newReporter
      STM.writeTVar boardReporters $ Map.insert rk reporter reporters
      return $ Right reporter
              

getReporter :: Board -> ReporterKey -> STM.STM (Maybe Reporter)
getReporter Board{..} rk = do
  reporters <- STM.readTVar boardReporters
  reporter <- return $ Map.lookup rk reporters
  return reporter




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
  



connectReporter :: Reporter -> WS.Connection -> STM.STM ConnKey
connectReporter Reporter{..} conn = do
  conns <- STM.readTVar reporterConnections
  key <- STM.readTVar reporterNextConnKey
  STM.writeTVar reporterConnections $ Map.insert key conn conns
  let key' = key +1
  STM.writeTVar reporterNextConnKey key'
  aha <- STM.readTVar reporterAha
  STM.writeTChan reporterChan $ MessageAha aha
  return key'
      

disconnectReporter :: Reporter -> ConnKey -> STM.STM ()
disconnectReporter Reporter{..} ck = do
  conns <- STM.readTVar reporterConnections
  STM.writeTVar reporterConnections $ Map.delete ck conns
  


connectViewer :: Board -> WS.Connection -> STM.STM (ViewerKey, Viewer)
connectViewer = undefined

disconnectViewer :: Board -> ViewerKey -> STM.STM ()
disconnectViewer = undefined













main :: IO ()
main = do
  host:port:backupPath:_ <- getArgs
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
    mboard <- getBoard server secretKey
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

  mboard <- STM.atomically $ getBoard server secretKey
  board <- case mboard of
    Nothing -> throwError 10002 "\"secret_key\" is not found"
    Just board -> return board

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseBoard secretKey (boardPublicKey board) (boardCaption board) )




websocketApp :: Server -> WS.ServerApp
websocketApp = undefined
--websocketApp state pconn
--  | ("/viewer"   == path) = viewerServer state pconn
--  | ("/reporter" == path) = reporterServer state pconn
--  | otherwise = WS.rejectRequest pconn "request rejected"
--  where
--    requestPath = WS.requestPath $ WS.pendingRequest pconn
--    path = BS.takeWhile (/='?') requestPath
