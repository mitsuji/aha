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
import qualified Network.WebSockets as WS
import Control.Concurrent ( MVar, newMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Exception (finally)
import Control.Monad (forever, when)
import qualified Data.ByteString.Char8 as BS -- use for input 
import qualified Data.ByteString.Lazy.Char8 as LBS -- use for output
import Data.Monoid (mappend)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID

import Board
import ServerState



main :: IO ()
main = do
  args <- getArgs
  vstate <- newMVar ServerState.new
  Warp.runSettings (
    Warp.setHost ( fromString (args !! 0) ) $
    Warp.setPort ( read (args !! 1) ) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (websocketApp vstate) staticApp


staticApp :: Wai.Application
staticApp = do
  let settings = Static.embeddedSettings $(embedDir "static")
  let indices = fromJust $ toPieces $ ["board.html"]
  Static.staticApp $ settings { Static.ssIndices = indices }


websocketApp :: MVar ServerState -> WS.ServerApp
websocketApp state pconn
  -- [TODO] match exact path by equal
  | "/board"    `BS.isPrefixOf` requestPath = boardServer state pconn
  | "/reporter" `BS.isPrefixOf` requestPath = reporterServer state pconn
  | otherwise = WS.rejectRequest pconn "invalid path"
  where  
    requestPath = WS.requestPath $ WS.pendingRequest pconn


boardServer :: MVar ServerState -> WS.ServerApp
boardServer vstate pconn = do
  BS.putStrLn $ "boardServer: " `mappend` requestPath -- debug

  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  ---
  --- secretKey from url query
  ---
  let msecretKey = fmap BS.unpack $ Map.lookup "bk" $ Map.fromList query

  --
  -- resume or crete board from secretKey
  --
  (secretKey, publicKey, vboard) <- case msecretKey of
    Just sk -> do
      state <- readMVar vstate
      case publicKeyFromSecretKey state sk of
        Just pk -> resume state sk pk conn -- connect with valid secretKey
        Nothing -> create conn -- connect with invalid secretKey
    Nothing -> create conn -- connect without secretKey

  --
  -- send secretKey and pulicKey to client
  --
  WS.sendTextData conn $ boardKeysJson secretKey publicKey

  --
  -- websocket communication
  --
  finally ( boardTalk conn vboard ) $ disconnect vboard

  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    resume :: ServerState -> BoardSecretKey -> BoardPublicKey -> WS.Connection
              -> IO (BoardSecretKey, BoardPublicKey, MVar Board)
    resume state sk pk conn = do
      let vboard = fromJust $ boardFromPublicKey state pk -- [TODO] when Nothing
      board <- modifyMVar vboard $ \board -> case setConnection board conn of
        Left msg -> error msg
        Right board' -> return (board', board')
      -- debug
      putStrLn $ "resume board sk: " ++ sk ++ " pk: " ++ pk
      -- send current total to client
      WS.sendTextData conn $ totalJson $ totalAhaCount board
      return(sk, pk ,vboard)

    create :: WS.Connection -> IO (BoardSecretKey, BoardPublicKey, MVar Board)
    create conn = do
      bskUUID <- nextUUID
      bpkUUID <- nextUUID
      let sk = fromJust $ fmap UUID.toString bskUUID -- [TODO] when Nothing
      let pk = fromJust $ fmap UUID.toString bpkUUID -- [TODO] when Nothing
      vboard <- newMVar $ Board.new conn
      modifyMVar_ vstate $ \state -> return $ addBoard state sk pk vboard
      putStrLn $ "create board: sk: " ++ sk ++ " pk: " ++ pk
      return(sk, pk ,vboard)

    boardKeysJson sk pk = "{\"type\":\"bk\","
                          `mappend` "\"bsk\":\"" `mappend` LBS.pack sk `mappend` "\"," 
                          `mappend` "\"bpk\":\"" `mappend` LBS.pack pk `mappend` "\"" 
                          `mappend` "}"

    disconnect vboard = modifyMVar_ vboard $ \board -> return $ closeConnection board


boardTalk :: WS.Connection -> MVar Board -> IO ()
boardTalk conn vboard = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
--  BS.putStrLn $ "msg master: " `BS.append` BS.pack (show mk) `BS.append` ": " `BS.append` msg

  --
  -- reset event
  --
  
  -- change state
  board <- modifyMVar vboard $ \board -> do
    let board' = reset board
    return (board',board')
  -- broadcast reset to reporters
  mapM_ (\rconn-> WS.sendTextData rconn resetJson) $ reporterConnections board 
  -- send reset to self
  WS.sendTextData conn resetJson
  
  where
    resetJson = "{\"type\":\"reset\"}" :: LBS.ByteString
  


reporterServer :: MVar ServerState.ServerState -> WS.ServerApp
reporterServer vstate pconn = do
  BS.putStrLn $ "reporterServer: " `mappend` requestPath -- debug

  -- [TODO] when Nothing
  let boardPublicKey = fromJust $ fmap BS.unpack $ Map.lookup "bk" $ Map.fromList query

  --
  -- resume board from boardPublicKey
  --
  state <- readMVar vstate
  -- [TODO] whtn Nothing
  let vboard = fromJust $ boardFromPublicKey state boardPublicKey

  
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  ---
  --- secretKey from url query
  ---
  let mreporterSecretKey = fmap BS.unpack $ Map.lookup "rk" $ Map.fromList query

  --
  -- resume or crete reporter from secretKey
  --
  reporterSecretKey <- case mreporterSecretKey of
    Just rsk -> do
      board <- readMVar vboard
      case reporter board rsk of
        True -> resume vboard rsk conn -- connect with valid secretKey
        False -> create vboard conn -- connect with invalid secretKey
    Nothing -> create vboard conn -- connect without secretKey

  --
  -- send secretKey to client
  --
  WS.sendTextData conn $ reporterKeyJson reporterSecretKey

  ---
  --- websocket communication
  ---
  finally (reporterTalk conn vboard reporterSecretKey) $ disconnect vboard reporterSecretKey
    
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    resume :: MVar Board -> ReporterKey -> WS.Connection -> IO ReporterKey
    resume vboard rk conn = do
      board <- modifyMVar vboard $ \board -> case setReporterConnection board rk conn of
        Left msg -> error msg
        Right board' -> return (board', board')
      -- debug
      putStrLn $ "resume reporter: rk: " ++ rk
      -- send current ahaCount to client
      case reporterAhaCount board rk of
        Left msg -> error msg
        Right count -> WS.sendTextData conn $ ahaCountJson count
      return rk

    create :: MVar Board -> WS.Connection -> IO ReporterKey
    create vboard conn = do
      rkUUID <- nextUUID
      let rk = fromJust $ fmap UUID.toString rkUUID -- [TODO] when Nothing
      modifyMVar_ vboard $ \board -> return $ addReporter board rk conn
      putStrLn $ "create reporter: rk: " ++ rk
      return rk
        
    reporterKeyJson rk = "{\"type\":\"rk\","
                         `mappend` "\"content\":\"" `mappend` LBS.pack rk `mappend` "\""
                         `mappend` "}"
                         
    disconnect vboard rk =
      modifyMVar_ vboard $ \board -> case closeReporterConnection board rk of
        Left msg -> error msg
        Right board' -> return board'


reporterTalk :: WS.Connection -> MVar Board -> ReporterKey -> IO ()
reporterTalk conn vboard reporterSecretKey = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
--  BS.putStrLn $ "msg slave: " `BS.append` BS.pack (show mk) `BS.append` ": " `BS.append` BS.pack (show sk) `BS.append` ": " `BS.append` msg

  --
  -- aha event
  --

  -- change state
  (board, ahaCount, total) <- modifyMVar vboard $ \board -> case aha board reporterSecretKey of
    Left msg -> error msg
    Right result@(board', _, _) -> return (board', result)
                              
  -- send total to board
  let bconn = connection board
  when (isJust bconn) $ WS.sendTextData (fromJust bconn) $ totalJson total
  -- send count to self
  WS.sendTextData conn $ ahaCountJson ahaCount
    

ahaCountJson :: Int -> LBS.ByteString
ahaCountJson count = "{\"type\": \"ahaCount\","
                     `mappend` "\"content\": " `mappend` LBS.pack (show count )
                     `mappend` "}"


totalJson :: Int -> LBS.ByteString
totalJson count = "{\"type\": \"total\","
                  `mappend` "\"content\": " `mappend` LBS.pack (show count )
                  `mappend` "}"
