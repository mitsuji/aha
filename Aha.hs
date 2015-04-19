{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import WaiAppStatic.Types (toPieces)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Network.Wai.Handler.WebSockets (websocketsOr)
import Data.FileEmbed (embedDir)
import Control.Monad (forever, when)
import Control.Concurrent (forkIO, MVar, newMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Exception (catch, finally, SomeException, ErrorCall)   
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Monoid (mappend)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Data.List (foldl')
import System.Environment (getArgs)
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
    -- [TODO] setHost
    Warp.setPort ( read (args !! 0) ) Warp.defaultSettings)
    $ websocketsOr WS.defaultConnectionOptions (application vstate) staticApp


staticApp :: Wai.Application
staticApp = do
  let settings = Static.embeddedSettings $(embedDir "static")
  let indices = fromJust $ toPieces $ ["board.html"]
  Static.staticApp $ settings { Static.ssIndices = indices  }


application :: MVar ServerState -> WS.ServerApp
application state pconn
  | "/board"    `BS.isPrefixOf` requestPath = boardServer state pconn
  | "/reporter" `BS.isPrefixOf` requestPath = reporterServer state pconn
  | otherwise = WS.rejectRequest pconn "invalid path"
  where  
    requestPath = WS.requestPath $ WS.pendingRequest pconn



boardServer :: MVar ServerState -> WS.ServerApp
boardServer vstate pconn = do
  BS.putStrLn requestPath

  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  --
  -- resume or crete board from bk
  --
  (bsk, bpk, vboard) <- case mbk of
    Just (bsk) -> do
      state <- readMVar vstate
      case publicKeyFromSecretKey state bsk of
        Just (bpk) -> resume state bsk bpk conn -- valid bk
        Nothing -> create conn -- invalid bk
    Nothing -> create conn -- no bk
  
  WS.sendTextData conn $ boardKeyJson bsk bpk
      
  flip finally (disconnect vboard) $ do
    boardTalk conn vboard

  where
    boardKeyJson bsk bpk = "{\"type\":\"bk\","
                           `mappend` "\"bsk\":\"" `mappend` LBS.pack bsk `mappend` "\"," 
                           `mappend` "\"bpk\":\"" `mappend` LBS.pack bpk `mappend` "\"" 
                           `mappend` "}"
    mbk = fmap BS.unpack $ Map.lookup "bk" $ Map.fromList query
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    resume state bsk bpk conn = do
      let vboard = fromJust $ boardFromPublicKey state bpk
      board <- modifyMVar vboard $ \board -> do
        let board' = setConnection board conn
        return (board',board')
      putStrLn $ "resume board: " ++ bsk ++ ": " ++ bpk
      WS.sendTextData conn $ totalJson $ boardAhaCount board
      return(bsk, bpk ,vboard)
    create conn = do
      bskUUID <- nextUUID
      bpkUUID <- nextUUID
      let bsk = fromJust $ fmap UUID.toString bskUUID 
      let bpk = fromJust $ fmap UUID.toString bpkUUID 
      vboard <- newMVar $ Board.new conn
      modifyMVar_ vstate $ \state -> return $ addBoard state bsk bpk vboard
      putStrLn $ "create board: " ++ bsk ++ ": " ++ bpk
      return(bsk, bpk ,vboard)
    disconnect vboard = modifyMVar_ vboard $ \board -> return $ closeConnection board


boardTalk :: WS.Connection -> MVar Board -> IO ()
boardTalk conn vboard = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
--  BS.putStrLn $ "msg master: " `BS.append` BS.pack (show mk) `BS.append` ": " `BS.append` msg

  board <- modifyMVar vboard $ \board -> do
    let board' = reset board
    return (board',board')

  -- broadcast to reporters
  mapM_ (\rconn-> WS.sendTextData rconn resetJson) $ reporterConnections board 
  -- send to self
  WS.sendTextData conn resetJson
  
  where
    resetJson = "{\"type\":\"reset\"}" :: LBS.ByteString



reporterServer :: MVar ServerState.ServerState -> WS.ServerApp
reporterServer vstate pconn = do
  BS.putStrLn requestPath

  let bpk = fromJust mbk 

  --
  -- board from bk
  --
  state <- readMVar vstate
  let vboard = fromJust $ boardFromPublicKey state bpk

  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  --
  -- resume or crete reporter from rk
  --
  rk <- case mrk of
    Just(rk) -> do
      board <- readMVar vboard
      case reporter board rk of
        True -> resume vboard rk conn
        False -> create vboard conn
    Nothing -> create vboard conn

  WS.sendTextData conn $ reporterKeyJson rk
    
  flip finally (disconnect vboard rk) $ do
    reporterTalk conn vboard rk
  where
    reporterKeyJson rk = "{\"type\":\"rk\","
                         `mappend` "\"content\":\"" `mappend` LBS.pack rk `mappend` "\""
                         `mappend` "}"
    mbk = fmap BS.unpack $ Map.lookup "bk" $ Map.fromList query
    mrk = fmap BS.unpack $ Map.lookup "rk" $ Map.fromList query
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    resume vboard rk conn = do
      board <- modifyMVar vboard $ \board -> do
        let board' = setReporterConnection board rk conn
        return (board', board')
      putStrLn $ "resume reporter: " ++ rk
      WS.sendTextData conn $ ahaCountJson $ reporterAhaCount board rk
      return rk
    create vboard conn = do
      rkUUID <- nextUUID
      let rk = fromJust $ fmap UUID.toString rkUUID 
      modifyMVar_ vboard $ \board -> return $ addReporter board rk conn
      putStrLn $ "create reporter: " ++ rk
      return rk
    disconnect vboard rk =
      modifyMVar_ vboard $ \board -> return $ closeReporterConnection board rk


reporterTalk :: WS.Connection -> MVar Board -> ReporterKey -> IO ()
reporterTalk conn vboard rk = forever $ do
  msg <- WS.receiveData conn :: IO BS.ByteString
--  BS.putStrLn $ "msg slave: " `BS.append` BS.pack (show mk) `BS.append` ": " `BS.append` BS.pack (show sk) `BS.append` ": " `BS.append` msg

  (board, ahaCount, total) <- modifyMVar vboard $ \board -> do
    let r@(board', _, _) = aha board rk
    return (board', r)
    
  -- board connection
  let bconn = connection board
  -- send new total to board
  when (isJust bconn) $ WS.sendTextData (fromJust bconn) $ totalJson total
  -- send new count to reporter
  WS.sendTextData conn $ ahaCountJson ahaCount
    


ahaCountJson :: Int -> LBS.ByteString
ahaCountJson count = "{\"type\": \"ahaCount\","
                     `mappend` "\"content\": " `mappend` LBS.pack (show count )
                     `mappend` "}"


totalJson :: Int -> LBS.ByteString
totalJson count = "{\"type\": \"total\","
                  `mappend` "\"content\": " `mappend` LBS.pack (show count )
                  `mappend` "}"
