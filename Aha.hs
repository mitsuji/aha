{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Concurrent (ThreadId, forkIO, threadDelay, killThread, MVar, newMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Exception (throwIO, finally)
import System.IO.Error(IOError,userError,catchIOError,ioeGetErrorString)
import Control.Monad (forever, when)
import qualified Data.ByteString.Char8 as BS -- use for input 
import qualified Data.ByteString.Lazy.Char8 as LBS -- use for output
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, fromJust)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID

import Data.Aeson.Types(ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE
import System.IO(withFile,IOMode(ReadMode,WriteMode))

import System.Posix.Signals(installHandler,sigINT,sigTERM,Handler(..))

import Board
import ServerState

type AhaCount = Int
type TotalAhaCount = Int

data Response = ResponseError Int String
              | ResponseBoardKeys BoardSecretKey BoardPublicKey Caption TotalAhaCount
              | ResponseReporterKeys ReporterKey BoardPublicKey Caption AhaCount TotalAhaCount
              deriving (Show)

data Message = MessageReset
             | MessageAhaCount AhaCount
             | MessageTotalAhaCount TotalAhaCount
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

restore' :: MVar ServerState -> String -> IO()
restore' vstate path = do
  putStrLn "restore"
  json <- withFile path ReadMode ( \h -> BS.hGetContents h )
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




webApp :: MVar ServerState -> Wai.Application
webApp vstate req respond
  | ["addBoard"]    == path = catchIOError (addBoardProc    vstate req respond) onError
  | ["getBoard"]    == path = catchIOError (getBoardProc    vstate req respond) onError
  | ["addReporter"] == path = catchIOError (addReporterProc vstate req respond) onError
  | ["getReporter"] == path = catchIOError (getReporterProc vstate req respond) onError
  | otherwise = staticApp req respond -- static html/js/css files
  where
    path = Wai.pathInfo req
    
    onError :: IOError -> IO Wai.ResponseReceived
    onError e = respond $ Wai.responseLBS
                H.status200
                [contentTypeJsonHeader]
                $ AE.encode $ ResponseError 999 $ ioeGetErrorString e


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.embeddedSettings $(embedDir "static") -- embed contents as ByteString
    indices = fromJust $ toPieces ["board.html"] -- default content


contentTypeJsonHeader :: H.Header
contentTypeJsonHeader = ("Content-Type","application/json")


throwError :: String -> IO a
throwError msg = throwIO $ userError msg


addBoardProc :: MVar ServerState -> Wai.Application
addBoardProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters
  
  msk         <- nextUUID -- generate UUID for boardSsecretKey
  let mpk      = Map.lookup "pk"      $ Map.fromList params -- publicKey
  let mcaption = Map.lookup "caption" $ Map.fromList params -- caption

  when (isNothing msk)      $ throwError "'sk' is not generated"
  when (isNothing mpk)      $ throwError "'pk' is not specified"
  when (isNothing mcaption) $ throwError "'caption' is not specified"

  let sk      = UUID.toString $ fromJust msk
  let pk      = BS.unpack $ fromJust mpk
  let caption = BS.unpack $ fromJust mcaption

  vboard <- newMVar $ Board.new caption
  modifyMVar_ vstate $ \state ->
    case addBoard state sk pk vboard of
      Left error -> throwError $ show error
      Right board' -> return board'
    
  putStrLn $ "addBoard: sk: " ++ sk ++ " pk: " ++ pk ++ " caption: " ++ caption

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    $ AE.encode $ ResponseBoardKeys sk pk caption 0


getBoardProc :: MVar ServerState -> Wai.Application
getBoardProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  let msk = Map.lookup "sk" $ Map.fromList params -- secretKey
  when (isNothing msk) $ throwError "'sk' is not specified"
  let sk = BS.unpack $ fromJust msk

  state <- readMVar vstate
  (pk, board) <- case publicKeyFromSecretKey state sk of
    Nothing -> throwError "'sk' is not found"
    Just pk -> case boardFromPublicKey state pk of
      Nothing -> throwError "'pk' is not found"
      Just vboard -> readMVar vboard >>= \board -> return (pk,board)

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    $ AE.encode $ ResponseBoardKeys sk pk (caption board) (totalAhaCount board)




addReporterProc :: MVar ServerState -> Wai.Application
addReporterProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  mrsk     <- nextUUID -- generate UUID for reporterSecretKey
  let mbpk = Map.lookup "bpk" $ Map.fromList params

  when (isNothing mrsk) $ throwError "'rsk' is not generated"
  when (isNothing mbpk) $ throwError "'bpk' is not specified"

  let rsk = UUID.toString $ fromJust mrsk
  let bpk = BS.unpack $ fromJust mbpk

  state <- readMVar vstate
  board <- case boardFromPublicKey state bpk of
    Nothing -> throwError "'bpk' is not found"
    Just vboard -> modifyMVar vboard $ \board -> do
      let board' = addReporter board rsk
      return (board', board')
      
  putStrLn $ "addReporter: rsk: " ++ rsk ++ " bpk: " ++ bpk

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    $ AE.encode $ ResponseReporterKeys rsk bpk (caption board)
    0 (totalAhaCount board)
  
    
getReporterProc :: MVar ServerState -> Wai.Application
getReporterProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters
  
  let mrsk = Map.lookup "rsk" $ Map.fromList params
  let mbpk = Map.lookup "bpk" $ Map.fromList params
      
  when (isNothing mrsk) $ throwError "'rsk' is not specified"
  when (isNothing mbpk) $ throwError "'bpk' is not specified"

  let rsk = BS.unpack $ fromJust mrsk
  let bpk = BS.unpack $ fromJust mbpk

  state <- readMVar vstate
  board <- case boardFromPublicKey state bpk of
    Nothing -> throwError "'bpk' is not found"
    Just vboard -> do
      board <- readMVar vboard
      case hasReporter board rsk of
        False -> throwError "'rsk' is not found"
        True -> return board

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    $ AE.encode $ ResponseReporterKeys rsk bpk (caption board)
    (fromJust $ reporterAhaCount board rsk) (totalAhaCount board)




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
  when (isNothing msk) $ throwError "'bk' is not specified"
  let sk = BS.unpack $ fromJust msk

  vboard <- do
    state <- readMVar vstate
    case publicKeyFromSecretKey state sk of
      Nothing -> throwError "'bk' is not found"
      Just pk -> case boardFromPublicKey state pk of
        Nothing -> throwError "'pk' is not found"
        Just vboard -> return vboard

  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  modifyMVar_ vboard $ \board ->
    case setConnection board conn of
      Left error -> throwError $ show error
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

  when (isNothing mbpk) $ throwError "'bk' is not specified"
  when (isNothing mrsk) $ throwError "'rk' is not specified"

  let bpk = BS.unpack $ fromJust mbpk
  let rsk = BS.unpack $ fromJust mrsk
      
  state <- readMVar vstate
  vboard <- case boardFromPublicKey state bpk of
    Nothing -> throwError "'bk' is not found"
    Just vboard -> do
      board <- readMVar vboard
      case hasReporter board rsk of
        False -> throwError "'rk' is not found"
        True -> return vboard
  
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  modifyMVar_ vboard $ \board ->
    case setReporterConnection board rsk conn of
      Left error -> throwError $ show error
      Right board' -> return board'

  finally (reporterLoop conn vboard rsk) $ disconnect vboard rsk
    
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    disconnect vboard rk =
      modifyMVar_ vboard $ \board ->
        case closeReporterConnection board rk of
          Left error -> throwError $ show error
          Right board' -> return board'


reporterLoop :: WS.Connection -> MVar Board -> ReporterKey -> IO ()
reporterLoop conn vboard reporterSecretKey = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  
  (board, ahaCount) <- modifyMVar vboard $ \board ->
    case aha board reporterSecretKey of
      Left error -> throwError $ show error
      Right result@(board', _) -> return (board', result)
                              
  let bconn = connection board
  when (isJust bconn) $ WS.sendTextData (fromJust bconn) $ AE.encode $ MessageTotalAhaCount $ totalAhaCount board
  WS.sendTextData conn $ AE.encode $ MessageAhaCount ahaCount



