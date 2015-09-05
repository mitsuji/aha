{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String (fromString)
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
import Control.Concurrent (ThreadId,forkIO,threadDelay)
import Data.Typeable (Typeable)
import Control.Exception (Exception,catch,throwIO,throwTo,finally)
import Control.Monad (forever,void)
import qualified Data.ByteString.Char8 as BS -- use for input 
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Network.HTTP.Types.URI (parseSimpleQuery)
import qualified Data.Aeson as AE

import System.IO(withFile,IOMode(ReadMode,WriteMode))
import System.Posix.Signals(installHandler,sigINT,sigTERM,Handler(Catch))

import Control.Applicative ((<$>),(<*>))
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async(race)

import Server
import JSON







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


