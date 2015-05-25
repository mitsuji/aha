{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Control.Concurrent (ThreadId,forkIO,threadDelay,killThread,MVar,newMVar,readMVar,modifyMVar,modifyMVar_)
import Data.Typeable(Typeable)
import Control.Exception (Exception,catch,throwIO,finally)
import Control.Monad (forever, when)
import qualified Data.ByteString.Char8 as BS -- use for input 
import qualified Data.ByteString.Lazy.Char8 as LBS -- use for output
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust,isNothing,fromJust)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID

import Data.Aeson.Types(ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE
import System.IO(withFile,IOMode(ReadMode,WriteMode))

import System.Posix.Signals(installHandler,sigINT,sigTERM,Handler(..))

import Board
import ServerState





main :: IO ()
main = do
  host:port:backupPath:_ <- getArgs
  vstate <- newMVar ServerState.new
  restore' vstate backupPath
  threadBackup <- forkIO $ backup vstate backupPath -- fork backup thread
  Warp.runSettings (
    Warp.setHost ( fromString host ) $
    Warp.setPort ( read port ) $
-- [TODO] shutdown correctly
--    Warp.setInstallShutdownHandler ( installShutdownHandler threadBackup ) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (websocketApp vstate) (webApp vstate)




-- [TODO] shutdown correctly
installShutdownHandler :: ThreadId -> IO() -> IO()
installShutdownHandler threadBackup close = do
  installHandler sigINT (CatchOnce shutdown) Nothing
  installHandler sigTERM (CatchOnce shutdown) Nothing
  putStrLn "installShutdownHandler"
  where
    shutdown = do
      close
      killThread threadBackup -- stop backup thead
      putStrLn "shutdown"

-- [TODO] function name
restore' :: MVar ServerState -> String -> IO()
restore' vstate path = do
  putStrLn "restore"
  json <- withFile path ReadMode BS.hGetContents
  case AE.decodeStrict json of
    Nothing -> return () -- do nothing
    Just items -> do
      state' <- ServerState.restore items
      modifyMVar_ vstate $ \_ -> return state'

backup ::  MVar ServerState -> String -> IO()
backup vstate path = forever $ do
  putStrLn "backup"
  state <- readMVar vstate
  items <- ServerState.dump state
  withFile path WriteMode ( \h -> LBS.hPutStrLn h $ AE.encode items )
  threadDelay $ 30 * 1000 * 1000




type ErrorCode = Int
type ErrorMessage = String

data AhaException = AhaException ErrorCode ErrorMessage deriving (Show,Typeable)
instance Exception AhaException

throwError :: ErrorCode -> ErrorMessage -> IO a
throwError code msg = throwIO $ AhaException code msg




data Response = ResponseError ErrorCode ErrorMessage
              | ResponseBoard BoardSecretKey BoardPublicKey Caption TotalAha
              | ResponseReporter ReporterKey Aha BoardPublicKey Caption TotalAha
              deriving (Show)

instance ToJSON Response where
  toJSON (ResponseError code msg) =
    object ["success"    .= False
           ,"error_code" .= code
           ,"message"    .= msg
           ]
  toJSON (ResponseBoard sk pk c ta) =
    object ["success" .= True
           ,"type"    .= ("board" :: String)
           ,"content" .= object ["secret_key" .= sk
                                ,"public_key" .= pk
                                ,"caption"    .= c
                                ,"total_aha"  .= ta
                                ]
           ]
  toJSON (ResponseReporter rk aha bpk c ta) =
    object ["success" .= True
           ,"type"    .= ("reporter" :: String)
           ,"content" .= object ["reporter_key"     .= rk
                                ,"aha"              .= aha
                                ,"board_public_key" .= bpk
                                ,"board_caption"    .= c
                                ,"board_total_aha"  .= ta
                                ]
           ]


webApp :: MVar ServerState -> Wai.Application
webApp vstate req respond
  | (["add_board"]    == path) = (addBoardProc    vstate req respond) `catch` onError
  | (["get_board"]    == path) = (getBoardProc    vstate req respond) `catch` onError
  | (["add_reporter"] == path) = (addReporterProc vstate req respond) `catch` onError
  | (["get_reporter"] == path) = (getReporterProc vstate req respond) `catch` onError
  | otherwise = staticApp req respond -- static html/js/css files
  where
    path = Wai.pathInfo req

    onError :: AhaException -> IO Wai.ResponseReceived
    onError e@(AhaException code msg) =
      respond $ Wai.responseLBS
      H.status200
      [contentTypeJsonHeader]
      (AE.encode $ ResponseError code msg)


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.embeddedSettings $(embedDir "static") -- embed contents as ByteString
    indices = fromJust $ toPieces ["board.html"] -- default content


contentTypeJsonHeader :: H.Header
contentTypeJsonHeader = ("Content-Type","application/json")


addBoardProc :: MVar ServerState -> Wai.Application
addBoardProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters
  
  msecretKey <- nextUUID -- generate UUID for secretKey
  let mpublicKey = Map.lookup "public_key" $ Map.fromList params -- publicKey
  let mcaption   = Map.lookup "caption"    $ Map.fromList params -- caption

  when (isNothing msecretKey) $ throwError 10001 "\"secret_key\" is not generated"
  when (isNothing mpublicKey) $ throwError 10002 "\"public_key\" is not specified"
  when (isNothing mcaption)   $ throwError 10003 "\"caption\" is not specified"

  let secretKey = UUID.toString $ fromJust msecretKey
  let publicKey = BS.unpack $ fromJust mpublicKey
  let caption   = BS.unpack $ fromJust mcaption

  vboard <- case Board.new caption of
    Left err -> throwError 10004 ("Board.new failed: " ++ (show err))
    Right board -> newMVar board

  modifyMVar_ vstate $ \state ->
    case ServerState.addBoard state secretKey publicKey vboard of
      Left err@BoardSecretKeyDuplicated -> throwError 10005 ("ServerState.addBoard failed: " ++ (show err))
      Left err@BoardPublicKeyDuplicated -> throwError 10006 ("ServerState.addBoard failed: " ++ (show err))
      Left err@BoardSecretKeyInvalid    -> throwError 10007 ("ServerState.addBoard failed: " ++ (show err))
      Left err@BoardPublicKeyInvalid    -> throwError 10008 ("ServerState.addBoard failed: " ++ (show err))
      Right board' -> return board'
    
  putStrLn $ "ServerState.addBoard: secretKey: " ++ secretKey ++ " publicKey: " ++ publicKey ++ " caption: " ++ caption

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseBoard secretKey publicKey caption 0 )


getBoardProc :: MVar ServerState -> Wai.Application
getBoardProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  let msecretKey = Map.lookup "secret_key" $ Map.fromList params -- secretKey
  when (isNothing msecretKey) $ throwError 10001 "\"secret_key\" is not specified"
  let secretKey = BS.unpack $ fromJust msecretKey

  state <- readMVar vstate
  publicKey <- case ServerState.publicKeyFromSecretKey state secretKey of
    Nothing -> throwError 10002 "\"secret_key\" is not found"
    Just publicKey -> return publicKey
  board <- case ServerState.boardFromPublicKey state publicKey of
    Nothing -> throwError 10003 "\"public_key\" is not found"
    Just vboard -> do
      board <-readMVar vboard
      return board

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseBoard secretKey publicKey (Board.caption board) (Board.aha board) )




addReporterProc :: MVar ServerState -> Wai.Application
addReporterProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  mreporterKey <- nextUUID -- generate UUID for reporterKey
  let mboardPublicKey = Map.lookup "board_public_key" $ Map.fromList params

  when (isNothing mreporterKey)    $ throwError 10001 "\"reporter_key\" is not generated"
  when (isNothing mboardPublicKey) $ throwError 10002 "\"board_public_key\" is not specified"

  let reporterKey    = UUID.toString $ fromJust mreporterKey
  let boardPublicKey = BS.unpack $ fromJust mboardPublicKey

  state <- readMVar vstate
  board <- case ServerState.boardFromPublicKey state boardPublicKey of
    Nothing -> throwError 10003 "\"board_public_key\" is not found"
    Just vboard -> modifyMVar vboard $ \board -> do
      case Board.addReporter board reporterKey of
        Left err -> throwError 10004 ("Board.addReporter failed: " ++ (show err))
        Right board' -> return (board', board')
      
  putStrLn $ "Board.addReporter: reporterKey: " ++ reporterKey ++ " boardPublicKey: " ++ boardPublicKey
  
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseReporter reporterKey 0 boardPublicKey (Board.caption board) (Board.aha board) )
  
    
getReporterProc :: MVar ServerState -> Wai.Application
getReporterProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters
  
  let mreporterKey    = Map.lookup "reporter_key"     $ Map.fromList params
  let mboardPublicKey = Map.lookup "board_public_key" $ Map.fromList params
      
  when (isNothing mreporterKey)    $ throwError 10001 "\"reporter_key\" is not specified"
  when (isNothing mboardPublicKey) $ throwError 10002 "\"board_public_key\" is not specified"

  let reporterKey    = BS.unpack $ fromJust mreporterKey
  let boardPublicKey = BS.unpack $ fromJust mboardPublicKey

  state <- readMVar vstate
  board <- case ServerState.boardFromPublicKey state boardPublicKey of
    Nothing -> throwError 10003 "\"board_public_key\" is not found"
    Just vboard -> do
      board <- readMVar vboard
      case Board.hasReporter board reporterKey of
        False -> throwError 10004 "\"reporter_key\" is not found"
        True -> return board
  raha <- case Board.reporterAha board reporterKey of
    Left err -> throwError 10005 ("Board.reporterAha failed: " ++ (show err))
    Right aha -> return aha

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseReporter reporterKey raha boardPublicKey (Board.caption board) (Board.aha board) )




data Message = MessageReset
             | MessageAha Aha
             | MessageTotalAha TotalAha
             deriving (Show)
                       
instance ToJSON Message where
  toJSON (MessageReset) =
    object ["type" .= ("reset" :: String)]
  toJSON (MessageAha aha) =
    object ["type"    .= ("aha" :: String)
           ,"content" .= aha
           ]
  toJSON (MessageTotalAha ta) =
    object ["type"    .= ("total_aha" :: String)
           ,"content" .= ta
           ]


-- [TODO] response error
-- [TODO] websocket error
websocketApp :: MVar ServerState -> WS.ServerApp
websocketApp state pconn
  | ("/board"    == path) = boardServer state pconn
  | ("/reporter" == path) = reporterServer state pconn
  | otherwise = WS.rejectRequest pconn "request rejected"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/='?') requestPath


boardServer :: MVar ServerState -> WS.ServerApp
boardServer vstate pconn = do
  putStrLn $ "boardServer: " ++ BS.unpack(requestPath) -- debug

  let msecretKey = Map.lookup "secret_key" $ Map.fromList query
  when (isNothing msecretKey) $ throwError 20001 "\"secret_key\" is not specified"
  let secretKey = BS.unpack $ fromJust msecretKey

  vboard <- do
    state <- readMVar vstate
    case ServerState.publicKeyFromSecretKey state secretKey of
      Nothing -> throwError 20002 "\"secret_key\" is not found"
      Just publicKey -> case ServerState.boardFromPublicKey state publicKey of
        Nothing -> throwError 20003 "\"public_key\" is not found"
        Just vboard -> return vboard

  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  modifyMVar_ vboard $ \board ->
    case Board.setConnection board conn of
      Left err -> throwError 20004 ("Board.setConnection failed: " ++ (show err))
      Right board' -> return board'

  finally ( boardLoop conn vboard ) $ disconnect vboard
  
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath
    disconnect vboard = modifyMVar_ vboard $ \board -> return $ Board.closeConnection board


boardLoop :: WS.Connection -> MVar Board -> IO ()
boardLoop conn vboard = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  
  board <- modifyMVar vboard $ \board -> do
    let board' = Board.reset board
    return (board',board')

  mapM_ (\c-> WS.sendTextData c $ AE.encode MessageReset) (Board.reporterConnections board) 
  WS.sendTextData conn $ AE.encode MessageReset
  
    
  

reporterServer :: MVar ServerState.ServerState -> WS.ServerApp
reporterServer vstate pconn = do
  putStrLn $ "reporterServer: " ++ BS.unpack(requestPath) -- debug

  let mboardPublicKey = Map.lookup "board_public_key" $ Map.fromList query
  let mreporterKey    = Map.lookup "reporter_key"     $ Map.fromList query

  when (isNothing mboardPublicKey) $ throwError 20001 "\"board_public_key\" is not specified"
  when (isNothing mreporterKey)    $ throwError 20002 "\"reporter_key\" is not specified"

  let boardPublicKey = BS.unpack $ fromJust mboardPublicKey
  let reporterKey    = BS.unpack $ fromJust mreporterKey
      
  state <- readMVar vstate
  vboard <- case ServerState.boardFromPublicKey state boardPublicKey of
    Nothing -> throwError 20003 "\"board_public_key\" is not found"
    Just vboard -> do
      board <- readMVar vboard
      case Board.hasReporter board reporterKey of
        False -> throwError 20004 "\"reporter_key\" is not found"
        True -> return vboard
  
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30
  
  modifyMVar_ vboard $ \board ->
    case Board.setReporterConnection board reporterKey conn of
      Left err@ReporterNotFound       -> throwError 20005 ("Board.setReporterConnection failed: " ++ (show err))
      Left err@ActiveConnectionExists -> throwError 20006 ("Board.setReporterConnection failed: " ++ (show err))
      Right board' -> return board'

  finally (reporterLoop conn vboard reporterKey) $ disconnect vboard reporterKey
    
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    disconnect vboard rk =
      modifyMVar_ vboard $ \board ->
        case Board.closeReporterConnection board rk of
          Left err -> throwError 20007 ("Board.closeReporterConnection failed: " ++ (show err))
          Right board' -> return board'


reporterLoop :: WS.Connection -> MVar Board -> ReporterKey -> IO ()
reporterLoop conn vboard reporterKey = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  
  (board, aha, ta) <- modifyMVar vboard $ \board ->
    case Board.incrementReporterAha board reporterKey of
      Left err -> throwError 20008 ("Board.incrementReporterAha failed: " ++ (show err))
      Right result@(board', _, _) -> return (board', result)
                              
  WS.sendTextData conn $ AE.encode $ MessageAha aha
  let mbconn = Board.connection board
  when (isJust mbconn) $ WS.sendTextData (fromJust mbconn) $ AE.encode $ MessageTotalAha ta
  mapM_ (\c-> WS.sendTextData c $ AE.encode $ MessageTotalAha ta) (Board.reporterConnections board)



