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
-- [TODO]    
--    Warp.setInstallShutdownHandler ( installShutdownHandler threadBackup ) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (websocketApp vstate) (webApp vstate)




-- [TODO]
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




type AhaCount = Int
type TotalAhaCount = Int

data Response = ResponseError ErrorCode ErrorMessage
              | ResponseBoardKeys BoardSecretKey BoardPublicKey Caption TotalAhaCount
              | ResponseReporterKeys ReporterKey BoardPublicKey Caption AhaCount TotalAhaCount
              deriving (Show)

instance ToJSON Response where
  toJSON (ResponseError code msg) =
    object ["success"    .= False
           ,"error_code" .= code
           ,"message"    .= msg
           ]
  toJSON (ResponseBoardKeys bsk bpk cap total) =
    object ["success" .= True
           ,"type"    .= ("bks" :: String)
           ,"content" .= object ["sk"      .= bsk
                                ,"pk"      .= bpk
                                ,"caption" .= cap
                                ,"total"   .= total
                                ]
           ]
  toJSON (ResponseReporterKeys rsk bpk cap aha total) =
    object ["success" .= True
           ,"type"    .= ("rsk" :: String)
           ,"content" .= object ["rsk"      .= rsk
                                ,"bpk"      .= bpk
                                ,"ahaCount" .= aha
                                ,"caption"  .= cap
                                ,"total"    .= total
                                ]
           ]


webApp :: MVar ServerState -> Wai.Application
webApp vstate req respond
  | ["addBoard"]    == path = catch (addBoardProc    vstate req respond) onError
  | ["getBoard"]    == path = catch (getBoardProc    vstate req respond) onError
  | ["addReporter"] == path = catch (addReporterProc vstate req respond) onError
  | ["getReporter"] == path = catch (getReporterProc vstate req respond) onError
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
  
  msk <- nextUUID -- generate UUID for boardSsecretKey
  let mpk      = Map.lookup "pk"      $ Map.fromList params -- publicKey
  let mcaption = Map.lookup "caption" $ Map.fromList params -- caption

  when (isNothing msk)      $ throwError 10001 "\"sk\" is not generated"
  when (isNothing mpk)      $ throwError 10002 "\"pk\" is not specified"
  when (isNothing mcaption) $ throwError 10003 "\"caption\" is not specified"

  let sk      = UUID.toString $ fromJust msk
  let pk      = BS.unpack $ fromJust mpk
  let caption = BS.unpack $ fromJust mcaption

  vboard <- case Board.new caption of
    Left err -> throwError 10004 ("Board.new failed: " ++ (show err))
    Right board -> newMVar board

  modifyMVar_ vstate $ \state ->
    case addBoard state sk pk vboard of
      Left err@BoardSecretKeyDuplicated -> throwError 10005 ("ServerState.addBoard failed: " ++ (show err))
      Left err@BoardPublicKeyDuplicated -> throwError 10006 ("ServerState.addBoard failed: " ++ (show err))
      Left err@BoardSecretKeyInvalid    -> throwError 10007 ("ServerState.addBoard failed: " ++ (show err))
      Left err@BoardPublicKeyInvalid    -> throwError 10008 ("ServerState.addBoard failed: " ++ (show err))
      Right board' -> return board'
    
  putStrLn $ "addBoard: sk: " ++ sk ++ " pk: " ++ pk ++ " caption: " ++ caption

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseBoardKeys sk pk caption 0 )


getBoardProc :: MVar ServerState -> Wai.Application
getBoardProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  let msk = Map.lookup "sk" $ Map.fromList params -- secretKey
  when (isNothing msk) $ throwError 10001 "\"sk\" is not specified"
  let sk = BS.unpack $ fromJust msk

  state <- readMVar vstate
  (pk, board) <- case publicKeyFromSecretKey state sk of
    Nothing -> throwError 10002 "\"sk\" is not found"
    Just pk -> case boardFromPublicKey state pk of
      Nothing -> throwError 10003 "\"pk\" is not found"
      Just vboard -> readMVar vboard >>= \board -> return (pk,board)

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseBoardKeys sk pk (caption board) (aha board) )




addReporterProc :: MVar ServerState -> Wai.Application
addReporterProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  mrsk <- nextUUID -- generate UUID for reporterSecretKey
  let mbpk = Map.lookup "bpk" $ Map.fromList params

  when (isNothing mrsk) $ throwError 10001 "\"rsk\" is not generated"
  when (isNothing mbpk) $ throwError 10002 "\"bpk\" is not specified"

  let rsk = UUID.toString $ fromJust mrsk
  let bpk = BS.unpack $ fromJust mbpk

  state <- readMVar vstate
  board <- case boardFromPublicKey state bpk of
    Nothing -> throwError 10003 "\"bpk\" is not found"
    Just vboard -> modifyMVar vboard $ \board -> do
      case addReporter board rsk of
        Left err -> throwError 10004 ("Board.addReporter failed: " ++ (show err))
        Right board' -> return (board', board')
      
  putStrLn $ "addReporter: rsk: " ++ rsk ++ " bpk: " ++ bpk

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseReporterKeys rsk bpk (caption board) 0 (aha board) )
  
    
getReporterProc :: MVar ServerState -> Wai.Application
getReporterProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters
  
  let mrsk = Map.lookup "rsk" $ Map.fromList params
  let mbpk = Map.lookup "bpk" $ Map.fromList params
      
  when (isNothing mrsk) $ throwError 10001 "\"rsk\" is not specified"
  when (isNothing mbpk) $ throwError 10002 "\"bpk\" is not specified"

  let rsk = BS.unpack $ fromJust mrsk
  let bpk = BS.unpack $ fromJust mbpk

  state <- readMVar vstate
  board <- case boardFromPublicKey state bpk of
    Nothing -> throwError 10003 "\"bpk\" is not found"
    Just vboard -> do
      board <- readMVar vboard
      case hasReporter board rsk of
        False -> throwError 10004 "\"rsk\" is not found"
        True -> return board

  raha <- case reporterAha board rsk of
    Left err -> throwError 10005 ("Board.reporterAha failed: " ++ (show err))
    Right aha -> return aha

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseReporterKeys rsk bpk (caption board) raha (aha board) )




data Message = MessageReset
             | MessageAhaCount AhaCount
             | MessageTotalAhaCount TotalAhaCount
             deriving (Show)
                       
instance ToJSON Message where
  toJSON (MessageReset) =
    object ["type" .= ("reset" :: String)]
  toJSON (MessageAhaCount count) =
    object ["type"    .= ("ahaCount" :: String)
           ,"content" .= count
           ]
  toJSON (MessageTotalAhaCount count) =
    object ["type"    .= ("total" :: String)
           ,"content" .= count
           ]


websocketApp :: MVar ServerState -> WS.ServerApp
websocketApp state pconn
  | "/board"    == path = boardServer state pconn
  | "/reporter" == path = reporterServer state pconn
  | otherwise = WS.rejectRequest pconn "request rejected"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/='?') requestPath


boardServer :: MVar ServerState -> WS.ServerApp
boardServer vstate pconn = do
  putStrLn $ "boardServer: " ++ BS.unpack(requestPath) -- debug

  let msk = Map.lookup "bk" $ Map.fromList query
  when (isNothing msk) $ throwError 20001 "\"bk\" is not specified"
  let sk = BS.unpack $ fromJust msk

  vboard <- do
    state <- readMVar vstate
    case publicKeyFromSecretKey state sk of
      Nothing -> throwError 20002 "\"bk\" is not found"
      Just pk -> case boardFromPublicKey state pk of
        Nothing -> throwError 20003 "\"pk\" is not found"
        Just vboard -> return vboard

  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  modifyMVar_ vboard $ \board ->
    case setConnection board conn of
      Left err -> throwError 20004 ("Board.setConnection failed: " ++ (show err))
      Right board' -> return board'

  finally ( boardLoop conn vboard ) $ disconnect vboard
  
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath
    disconnect vboard = modifyMVar_ vboard $ \board -> return $ closeConnection board


boardLoop :: WS.Connection -> MVar Board -> IO ()
boardLoop conn vboard = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  
  board <- modifyMVar vboard $ \board -> do
    let board' = reset board
    return (board',board')

  mapM_ (\rconn-> WS.sendTextData rconn $ AE.encode MessageReset) $ reporterConnections board 
  WS.sendTextData conn $ AE.encode MessageReset
  
    
  

reporterServer :: MVar ServerState.ServerState -> WS.ServerApp
reporterServer vstate pconn = do
  putStrLn $ "reporterServer: " ++ BS.unpack(requestPath) -- debug

  let mbpk = Map.lookup "bk" $ Map.fromList query
  let mrsk = Map.lookup "rk" $ Map.fromList query

  when (isNothing mbpk) $ throwError 20001 "\"bk\" is not specified"
  when (isNothing mrsk) $ throwError 20002 "\"rk\" is not specified"

  let bpk = BS.unpack $ fromJust mbpk
  let rsk = BS.unpack $ fromJust mrsk
      
  state <- readMVar vstate
  vboard <- case boardFromPublicKey state bpk of
    Nothing -> throwError 20003 "\"bk\" is not found"
    Just vboard -> do
      board <- readMVar vboard
      case hasReporter board rsk of
        False -> throwError 20004 "\"rk\" is not found"
        True -> return vboard
  
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  modifyMVar_ vboard $ \board ->
    case setReporterConnection board rsk conn of
      Left err@ReporterNotFound       -> throwError 20005 ("Board.setReporterConnection failed: " ++ (show err))
      Left err@ActiveConnectionExists -> throwError 20006 ("Board.setReporterConnection failed: " ++ (show err))
      Right board' -> return board'

  finally (reporterLoop conn vboard rsk) $ disconnect vboard rsk
    
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    disconnect vboard rk =
      modifyMVar_ vboard $ \board ->
        case closeReporterConnection board rk of
          Left err -> throwError 20007 ("Board.closeReporterConnection failed: " ++ (show err))
          Right board' -> return board'


reporterLoop :: WS.Connection -> MVar Board -> ReporterKey -> IO ()
reporterLoop conn vboard reporterSecretKey = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  
  (board, ahaCount) <- modifyMVar vboard $ \board ->
    case incrementReporterAha board reporterSecretKey of
      Left err -> throwError 20008 ("Board.incrementReporterAha failed: " ++ (show err))
      Right result@(board', _) -> return (board', result)
                              
  let bconn = connection board
  when (isJust bconn) $ WS.sendTextData (fromJust bconn) $ AE.encode $ MessageTotalAhaCount $ aha board
  WS.sendTextData conn $ AE.encode $ MessageAhaCount ahaCount



