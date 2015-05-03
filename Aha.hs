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
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Exception (throwIO, finally)
import System.IO.Error(userError)
import Control.Monad (forever, when)
import qualified Data.ByteString.Char8 as BS -- use for input 
import qualified Data.ByteString.Lazy.Char8 as LBS -- use for output
import Data.Monoid (mappend)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Functor ((<$>))
import Network.HTTP.Types.URI (parseSimpleQuery)
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID


import Board
import ServerState



main :: IO ()
main = do
  args <- getArgs
  vstate <- newMVar ServerState.new
--  threadBackup <- forkIO $ backupLoop vstate -- fork backup thread
  Warp.runSettings (
    Warp.setHost ( fromString (args !! 0) ) $
    Warp.setPort ( read (args !! 1) ) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (websocketApp vstate) (webApp vstate)
--  killThread threadBackup  -- stop backup thead




backupLoop :: MVar ServerState -> IO()
backupLoop vstate = forever $ do
  -- print all state with json format
  putStrLn $ "backup"
  state <- readMVar vstate
  let bs = ServerState.dump state
  mapM_ (\(sk,pk,vboard) -> do
            putStrLn $ sk ++ ":" ++ pk
            board <- readMVar vboard
            LBS.putStrLn $ Board.dump board
        ) bs
  threadDelay $ 3 * 1000 * 1000




webApp :: MVar ServerState -> Wai.Application
webApp vstate req respond
  | ["addBoard"]    == path = addBoardProc    vstate req respond
  | ["getBoard"]    == path = getBoardProc    vstate req respond
  | ["addReporter"] == path = addReporterProc vstate req respond
  | ["getReporter"] == path = getReporterProc vstate req respond
  | otherwise = staticApp req respond -- static html/js/css files
  where
    path = Wai.pathInfo req




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
  uuidSk <- nextUUID -- generate UUID for secretKey
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters
  
  --
  -- :t Maybe String
  --
  let msk      = UUID.toString <$> uuidSk -- secretKey
  let mpk      = (BS.unpack <$>) $ Map.lookup "pk"      $ Map.fromList params -- publicKey
  let mcaption = (BS.unpack <$>) $ Map.lookup "caption" $ Map.fromList params -- caption

  --
  -- check Nothing
  --
  when (isNothing msk)      $ throwError "\"sk\" is not generated"
  when (isNothing mpk)      $ throwError "\"pk\" is not specified"
  when (isNothing mcaption) $ throwError "\"caption\" is not specified"

  --
  -- :t String
  --
  let sk      = fromJust msk
  let pk      = fromJust mpk
  let caption = fromJust mcaption

  --
  -- add new board
  --
  vboard <- newMVar $ Board.new caption
  modifyMVar_ vstate $ \state ->
    case addBoard state sk pk vboard of
      Left msg -> throwError msg
      Right board' -> return board'
    
  putStrLn $ "addBoard: sk: " ++ sk ++ " pk: " ++ pk ++ " caption: " ++ caption

  --
  -- send response
  --
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    $ boardKeysJson sk pk caption 0


getBoardProc :: MVar ServerState -> Wai.Application
getBoardProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  --
  -- :t Maybe String
  --
  let msk = (BS.unpack <$>) $ Map.lookup "sk" $ Map.fromList params -- secretKey

  --
  -- check Nothing
  --
  when (isNothing msk) $ throwError "\"sk\" is not specified"

  --
  -- :t String
  --
  let sk = fromJust msk

  --
  -- get existing board
  --
  state <- readMVar vstate
  (pk, board) <- case publicKeyFromSecretKey state sk of
    Nothing -> throwError "\"sk\" is not found"
    Just pk -> case boardFromPublicKey state pk of
      Nothing -> throwError "\"pk\" is not found"
      Just vboard -> readMVar vboard >>= \board -> return (pk,board)

  --
  -- send response
  --
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    $ boardKeysJson sk pk (caption board) (totalAhaCount board)


boardKeysJson :: BoardSecretKey -> BoardPublicKey -> String -> Int -> LBS.ByteString
boardKeysJson sk pk caption total = "{\"type\":\"bks\","
                              `mappend` "\"sk\":\"" `mappend` LBS.pack sk `mappend` "\"," 
                              `mappend` "\"pk\":\"" `mappend` LBS.pack pk `mappend` "\"," 
                              `mappend` "\"caption\":\"" `mappend` LBS.pack caption `mappend` "\"," 
                              `mappend` "\"total\":\"" `mappend` LBS.pack (show total) `mappend` "\"" 
                              `mappend` "}"


addReporterProc :: MVar ServerState -> Wai.Application
addReporterProc vstate req respond = do
  uuidRsk <- nextUUID -- generate UUID for reporterSecretKey
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  --
  -- :t Maybe String
  --
  let mrsk = (UUID.toString <$>) uuidRsk
  let mbpk = (BS.unpack <$>) $ Map.lookup "bpk" $ Map.fromList params

  --
  -- check Nothing
  --
  when (isNothing mrsk) $ throwError "\"rsk\" is not generated"
  when (isNothing mbpk) $ throwError "\"bpk\" is not specified"

  --
  -- :t String
  --
  let rsk = fromJust mrsk
  let bpk = fromJust mbpk

  --
  -- add new reporter
  --
  state <- readMVar vstate
  board <- case boardFromPublicKey state bpk of
    Nothing -> throwError "\"bpk\" is not found"
    Just vboard -> modifyMVar vboard $ \board -> do
      let board' = addReporter board rsk
      return (board', board')
      
  putStrLn $ "addReporter: rsk: " ++ rsk ++ " bpk: " ++ bpk

  --
  -- send response
  --
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    $ reporterKeysJson rsk bpk 0 (caption board) (totalAhaCount board)
  
    
getReporterProc :: MVar ServerState -> Wai.Application
getReporterProc vstate req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters
  
  --
  -- :t Maybe String
  --
  let mrsk = (BS.unpack <$>) $ Map.lookup "rsk" $ Map.fromList params
  let mbpk = (BS.unpack <$>) $ Map.lookup "bpk" $ Map.fromList params
      
  --
  -- check Nothing
  --
  when (isNothing mrsk) $ throwError "\"rsk\" is not specified"
  when (isNothing mbpk) $ throwError "\"bpk\" is not specified"

  --
  -- :t String
  --
  let rsk = fromJust mrsk
  let bpk = fromJust mbpk

  --
  -- get existing reporter
  --
  state <- readMVar vstate
  board <- case boardFromPublicKey state bpk of
    Nothing -> throwError "\"bpk\" is not found"
    Just vboard -> do
      board <- readMVar vboard
      case hasReporter board rsk of
        False -> throwError "\"rsk\" is not found"
        True -> return board

  --
  -- send response
  --
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    $ reporterKeysJson rsk bpk (fromJust $ reporterAhaCount board rsk) (caption board) (totalAhaCount board)

  
reporterKeysJson rsk bpk ahaCount caption total = "{\"type\":\"rks\","
                     `mappend` "\"rsk\":\"" `mappend` LBS.pack rsk `mappend` "\","
                     `mappend` "\"bpk\":\"" `mappend` LBS.pack bpk `mappend` "\","
                     `mappend` "\"ahaCount\":\"" `mappend` LBS.pack (show ahaCount) `mappend` "\","
                     `mappend` "\"caption\":\"" `mappend` LBS.pack caption `mappend` "\","
                     `mappend` "\"total\":\"" `mappend` LBS.pack (show total) `mappend` "\""
                     `mappend` "}"




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
  putStrLn $ "boardServer: " `mappend` BS.unpack(requestPath) -- debug

  --
  -- :t Maybe String
  --
  let msk = (BS.unpack <$>) $ Map.lookup "bk" $ Map.fromList query

  --
  -- get board
  --
  vboard <- case msk of
    Nothing -> throwError "\"bk\" is not specified"
    Just sk -> do
      state <- readMVar vstate
      case publicKeyFromSecretKey state sk of
        Nothing -> throwError "\"bk\" is not found"
        Just pk -> case boardFromPublicKey state pk of
          Nothing -> throwError "\"pk\" is not found"
          Just vboard -> return vboard

  --
  -- get websocket connection
  --
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  --
  -- set connection
  --
  modifyMVar_ vboard $ \board ->
    case setConnection board conn of
      Left msg -> throwError msg
      Right board' -> return board'

  --
  -- start websocket communication
  --
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

  mapM_ (\rconn-> WS.sendTextData rconn resetJson) $ reporterConnections board 
  WS.sendTextData conn resetJson
  
  where
    resetJson = "{\"type\":\"reset\"}" :: LBS.ByteString
    
  


reporterServer :: MVar ServerState.ServerState -> WS.ServerApp
reporterServer vstate pconn = do
  putStrLn $ "reporterServer: " `mappend` BS.unpack(requestPath) -- debug

  --
  -- :t Maybe String
  --
  let mbpk = (BS.unpack <$>) $ Map.lookup "bk" $ Map.fromList query
  let mrsk = (BS.unpack <$>) $ Map.lookup "rk" $ Map.fromList query

  --
  -- check Nothing
  --
  when (isNothing mbpk) $ throwError "\"bk\" is not specified"
  when (isNothing mrsk) $ throwError "\"rk\" is not specified"

  --
  -- :t String
  --
  let bpk = fromJust mbpk
  let rsk = fromJust mrsk
      
  --
  -- get board and check reporterKey
  --
  state <- readMVar vstate
  vboard <- case boardFromPublicKey state bpk of
    Nothing -> throwError "\"bk\" is not found"
    Just vboard -> do
      board <- readMVar vboard
      case hasReporter board rsk of
        False -> throwError "\"rk\" is not found"
        True -> return vboard
  
  --
  -- get websocket connection
  --
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  --
  -- set connection
  --
  modifyMVar_ vboard $ \board ->
    case setReporterConnection board rsk conn of
      Left msg -> throwError msg
      Right board' -> return board'

  ---
  --- start websocket communication
  ---
  finally (reporterLoop conn vboard rsk) $ disconnect vboard rsk
    
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    disconnect vboard rk =
      modifyMVar_ vboard $ \board ->
        case closeReporterConnection board rk of
          Left msg -> throwError msg
          Right board' -> return board'


reporterLoop :: WS.Connection -> MVar Board -> ReporterKey -> IO ()
reporterLoop conn vboard reporterSecretKey = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
  
  (board, ahaCount) <- modifyMVar vboard $ \board ->
    case aha board reporterSecretKey of
      Left msg -> throwError msg
      Right result@(board', _) -> return (board', result)
                              
  let bconn = connection board
  when (isJust bconn) $ WS.sendTextData (fromJust bconn) $ totalJson $ totalAhaCount board
  WS.sendTextData conn $ ahaCountJson ahaCount

  where
    ahaCountJson :: Int -> LBS.ByteString
    ahaCountJson count = "{\"type\": \"ahaCount\","
                         `mappend` "\"content\": " `mappend` LBS.pack (show count )
                         `mappend` "}"

    totalJson :: Int -> LBS.ByteString
    totalJson count = "{\"type\": \"total\","
                      `mappend` "\"content\": " `mappend` LBS.pack (show count )
                      `mappend` "}"




