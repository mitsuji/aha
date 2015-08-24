{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}

import qualified Data.Map as Map
import qualified Control.Concurrent.STM as STM
import qualified Network.WebSockets as WS

import Data.Aeson.Types(ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE
import Control.Monad(when)
import "mtl" Control.Monad.Trans(lift)

import Data.Maybe(fromJust,isJust)

import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID


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
  { boardPublicKey     :: BoardPublicKey
  , boardCaption       :: Caption
  , boardAha           :: STM.TVar Int
  , boardNextViewerKey :: STM.TVar ViewerKey
  , boardViewers       :: STM.TVar (Map.Map ViewerKey Viewer)
  , boardReporters     :: STM.TVar (Map.Map ReporterKey Reporter)
  , boardChan           :: STM.TChan Message
  }

newBoard :: BoardPublicKey -> Caption -> STM.STM Board
newBoard bpk caption = do
  aha       <- STM.newTVar 0
  nvk       <- STM.newTVar 0
  viewers   <- STM.newTVar Map.empty
  reporters <- STM.newTVar Map.empty
  chan      <- STM.newBroadcastTChan 
  return Board { boardPublicKey     = bpk
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
           board <- newBoard bpk caption
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
