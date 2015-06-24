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
import Control.Concurrent (ThreadId,forkIO,threadDelay,MVar,newMVar,readMVar,modifyMVar,modifyMVar_)
import Data.Typeable(Typeable)
import Control.Exception (Exception,catch,throwIO,throwTo,finally)
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as BS -- use for input 
import qualified Data.ByteString.Lazy.Char8 as LBS -- use for output
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID

import Data.Aeson.Types(ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE
import System.IO(withFile,IOMode(ReadMode,WriteMode))

import System.Posix.Signals(installHandler,sigINT,sigTERM,Handler(Catch))

import Board
import ServerState





main :: IO ()
main = do
  host:port:backupPath:_ <- getArgs
  vstate <- newMVar ServerState.new
  restore' vstate backupPath
  threadBackup <- forkIO $ backup vstate backupPath -- fork backup thread
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.setInstallShutdownHandler (installShutdownHandler threadBackup) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (websocketApp vstate) (plainOldHttpApp vstate)

  
-- [TODO] change to UserInterrupt
data StopException = StopException deriving (Show,Typeable)
instance Exception StopException


installShutdownHandler :: ThreadId -> IO() -> IO()
installShutdownHandler threadBackup close = do
  void $ installHandler sigINT (Catch shutdown) Nothing
  void $ installHandler sigTERM (Catch shutdown) Nothing
  where
    shutdown = do
      -- [TODO] last backup before shutdown
      close
      throwTo threadBackup StopException


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
backup vstate path =
  -- [TODO] mask exception
  loop `catch` onException
  where
    loop = do 
      putStrLn "backup"
      state <- readMVar vstate
      items <- ServerState.dump state
      withFile path WriteMode ( \h -> LBS.hPutStrLn h $ AE.encode items )
      threadDelay $ 30 * 1000 * 1000
      loop

    onException :: StopException -> IO ()
    onException e = return ()



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


plainOldHttpApp :: MVar ServerState -> Wai.Application
plainOldHttpApp vstate req respond
  | (["reset_board"]  == path) = (resetBoardProc  vstate req respond) `catch` onError
  | (["add_board"]    == path) = (addBoardProc    vstate req respond) `catch` onError
  | (["get_board"]    == path) = (getBoardProc    vstate req respond) `catch` onError
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


resetBoardProc :: MVar ServerState -> Wai.Application
resetBoardProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  secretKey <- case Map.lookup "secret_key" $ Map.fromList params of
    Nothing -> throwError 10001 "\"secret_key\" is not specified"
    Just sk -> return $ T.unpack $ decodeUtf8 sk

  state <- readMVar vstate
  publicKey <- case ServerState.publicKeyFromSecretKey state secretKey of
    Nothing -> throwError 10002 "\"secret_key\" is not found"
    Just pk -> return pk
    
  board <- case ServerState.boardFromPublicKey state publicKey of
    Nothing -> throwError 10003 "\"public_key\" is not found"
    Just vboard -> modifyMVar vboard $ \board -> do
      let board' = Board.reset board
      return (board', board')

  mapM_ (\c-> WS.sendTextData c $ AE.encode MessageReset) (Board.viewerConnections board)
  mapM_ (\c-> WS.sendTextData c $ AE.encode MessageReset) (Board.allReporterConnections board)

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseReset )


addBoardProc :: MVar ServerState -> Wai.Application
addBoardProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  msecretKey <- nextUUID -- generate UUID for secretKey
  secretKey <- case msecretKey of
    Nothing -> throwError 10001 "\"secret_key\" is not generated"
    Just sk -> return $ UUID.toString sk

  publicKey <- case Map.lookup "public_key" $ Map.fromList params of
    Nothing -> throwError 10002 "\"public_key\" is not specified"
    Just pk -> return $ T.unpack $ decodeUtf8 pk

  caption <- case Map.lookup "caption" $ Map.fromList params of
    Nothing -> throwError 10003 "\"caption\" is not specified"
    Just ca -> return $ T.unpack $ decodeUtf8 ca

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
    (AE.encode $ ResponseBoard secretKey publicKey caption)


getBoardProc :: MVar ServerState -> Wai.Application
getBoardProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  secretKey <- case Map.lookup "secret_key" $ Map.fromList params of
    Nothing -> throwError 10001 "\"secret_key\" is not specified"
    Just sk -> return $ T.unpack $ decodeUtf8 sk

  state <- readMVar vstate
  publicKey <- case ServerState.publicKeyFromSecretKey state secretKey of
    Nothing -> throwError 10002 "\"secret_key\" is not found"
    Just pk -> return pk
    
  board <- case ServerState.boardFromPublicKey state publicKey of
    Nothing -> throwError 10003 "\"public_key\" is not found"
    Just vboard -> do
      board <-readMVar vboard
      return board

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseBoard secretKey publicKey (Board.caption board))








data Message = MessageBoard BoardPublicKey Caption
             | MessageReporter ReporterKey BoardPublicKey Caption
             | MessageAha Aha
             | MessageTotalAha TotalAha
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



-- [TODO] response error
-- [TODO] websocket error
websocketApp :: MVar ServerState -> WS.ServerApp
websocketApp state pconn
  | ("/viewer"   == path) = viewerServer state pconn
  | ("/reporter" == path) = reporterServer state pconn
  | otherwise = WS.rejectRequest pconn "request rejected"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/='?') requestPath


viewerServer :: MVar ServerState -> WS.ServerApp
viewerServer vstate pconn = do
  putStrLn $ "viewerServer: " ++ BS.unpack(requestPath) -- debug

  publicKey <- case Map.lookup "public_key" $ Map.fromList query of
    Nothing -> throwError 20001 "\"public_key\" is not specified"
    Just pk -> return $ T.unpack $ decodeUtf8 pk

  vboard <- do
    state <- readMVar vstate
    case ServerState.boardFromPublicKey state publicKey of
      Nothing -> throwError 20002 "\"public_key\" is not found"
      Just vb -> return vb

  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  (board,connKey) <- modifyMVar vboard $ \board -> do
    let r@(board',_) = Board.addViewerConnection board conn
    return (board',r)

  WS.sendTextData conn $ AE.encode $ MessageBoard publicKey (Board.caption board)
  WS.sendTextData conn $ AE.encode $ MessageTotalAha (Board.aha board)

  finally (viewerLoop conn vboard) $ disconnect vboard connKey
  
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath
    disconnect vboard connKey =
      modifyMVar_ vboard $ \board -> return $ Board.delViewerConnection board connKey


viewerLoop :: WS.Connection -> MVar Board -> IO ()
viewerLoop conn vboard = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  -- nothing to do
  return ()


  


    
  

reporterServer :: MVar ServerState.ServerState -> WS.ServerApp
reporterServer vstate pconn = do
  putStrLn $ "reporterServer: " ++ BS.unpack(requestPath) -- debug

  boardPublicKey <- case Map.lookup "board_public_key" $ Map.fromList query of
    Nothing -> throwError 20001 "\"board_public_key\" is not specified"
    Just bpk -> return $ T.unpack $ decodeUtf8 bpk

  state <- readMVar vstate
  vboard <- case ServerState.boardFromPublicKey state boardPublicKey of
    Nothing -> throwError 20002 "\"board_public_key\" is not found"
    Just vboard -> return vboard
  
  reporterKey <- case Map.lookup "reporter_key" $ Map.fromList query of
    Just (brk) -> do
      let rk = T.unpack $ decodeUtf8 brk
      board <- readMVar vboard
      case Board.hasReporter board rk of
        True -> return rk
        False -> addReporter' vboard
    Nothing -> addReporter' vboard
  
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30
  
  (board,connKey) <-modifyMVar vboard $ \board ->
    case Board.addReporterConnection board reporterKey conn of
      Left err@ReporterNotFound -> throwError 20005 ("Board.setReporterConnection failed: " ++ (show err))
      Right r@(board',_) -> return (board',r)

  WS.sendTextData conn $ AE.encode $ MessageReporter reporterKey boardPublicKey (Board.caption board)
  WS.sendTextData conn $ AE.encode $ MessageTotalAha (Board.aha board)
  
  case Board.reporterAha board reporterKey of
    Left err -> throwError 20006 ("Board.reporterAha failed: " ++ (show err))
    Right aha -> WS.sendTextData conn $ AE.encode $ MessageAha aha

  finally (reporterLoop conn vboard reporterKey) $ disconnect vboard reporterKey connKey
    
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    addReporter' vboard = do
      mreporterKey <- nextUUID
      reporterKey <- case mreporterKey of
        Nothing -> throwError 20003 "\"reporter_key\" is not generated"
        Just rk -> return $ UUID.toString rk
      modifyMVar_ vboard $ \board -> do
        case Board.addReporter board reporterKey of
          Left err -> throwError 20004 ("Board.addReporter failed: " ++ (show err))
          Right board' -> return board'
      return reporterKey                          

    disconnect vboard reporterKey connKey =
      modifyMVar_ vboard $ \board ->
      case Board.delReporterConnection board reporterKey connKey of
        Left err -> throwError 20007 ("Board.closeReporterConnection failed: " ++ (show err))
        Right board' -> return board'


reporterLoop :: WS.Connection -> MVar Board -> ReporterKey -> IO ()
reporterLoop conn vboard reporterKey = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  
  (board, aha, ta) <- modifyMVar vboard $ \board ->
    case Board.incrementReporterAha board reporterKey of
      Left err -> throwError 20007 ("Board.incrementReporterAha failed: " ++ (show err))
      Right r@(board', _, _) -> return (board', r)

  case Board.reporterConnections board reporterKey of
    Left err -> return ()
    Right conns -> mapM_ (\c-> WS.sendTextData c $ AE.encode $ MessageAha aha) conns

  mapM_ (\c-> WS.sendTextData c $ AE.encode $ MessageTotalAha ta) (Board.viewerConnections board)
  mapM_ (\c-> WS.sendTextData c $ AE.encode $ MessageTotalAha ta) (Board.allReporterConnections board)



