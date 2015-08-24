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

import Data.Maybe(fromJust)

type ConnKey = Int

-- reporterBroadcastChan
data Reporter = Reporter
  { reporterNextConnKey :: STM.TVar ConnKey
  , reporterConnections :: STM.TVar (Map.Map ConnKey WS.Connection)
  , reporterAha         :: STM.TVar Int
  , reporterSendChan    :: STM.TChan Message
  }

newReporter :: STM.STM Reporter
newReporter = do
  nck   <- STM.newTVar 0
  conns <- STM.newTVar Map.empty
  aha   <- STM.newTVar 0
  sc    <- STM.newTChan 
  return Reporter { reporterNextConnKey = nck
                  , reporterConnections = conns
                  , reporterAha         = aha
                  , reporterSendChan    = sc
                  }


-- viewerBroadcastChan
data Viewer = Viewer WS.Connection


type Caption = String
type ViewerKey = Int
type ReporterKey = String

data Board = Board
  { boardPublicKey     :: BoardPublicKey
  , boardCaption       :: Caption
  , boardNextViewerKey :: STM.TVar ViewerKey
  , boardViewers       :: STM.TVar (Map.Map ViewerKey Viewer)
  , boardReporters     :: STM.TVar (Map.Map ReporterKey Reporter)
  , boardBroadcastChan :: STM.TChan Message
  }

newBoard :: BoardPublicKey -> Caption -> STM.STM Board
newBoard bpk caption = do
  nvk       <- STM.newTVar 0
  viewers   <- STM.newTVar Map.empty
  reporters <- STM.newTVar Map.empty
  bc        <- STM.newBroadcastTChan 
  return Board { boardPublicKey     = bpk
               , boardCaption       = caption
               , boardNextViewerKey = nvk
               , boardViewers       = viewers
               , boardReporters     = reporters
               , boardBroadcastChan = bc
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
           | BoardPublicKeyDuplicated
           | BoardSecretKeyDuplicated
           deriving(Show)


checkAddBoard :: Server -> Caption -> BoardSecretKey -> BoardPublicKey -> IO (Either Error Board)
checkAddBoard Server{..} caption bsk bpk
  | not $ isValidCaption bpk   = return $ Left BoardCaptionInvalid
  | not $ isValidPublicKey bpk = return $ Left BoardPublicKeyInvalid
  | otherwise = STM.atomically $ do 
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
  where
    isValidCaption cand = 0 < length cand && length cand <= 20
    isValidPublicKey cand = (all (\c -> elem c "abcdefghijklmnopqrstuvwxyz0123456789") cand)
                            && (0 < length cand && length cand <= 20)


getBoard :: Server -> BoardSecretKey -> IO (Maybe Board)
getBoard Server{..} bsk = STM.atomically $ do
  keys <- STM.readTVar serverBoardKeys
  boards <- STM.readTVar serverBoards
  bpk <- return $ Map.lookup bsk keys
  board <- return $ Map.lookup (fromJust bpk) boards
  return $ board




--data Response = ResponseError ErrorCode ErrorMessage
--              | ResponseBoard BoardSecretKey BoardPublicKey Caption
--              | ResponseReset
--              deriving (Show)
--
--instance ToJSON Response where
--  toJSON (ResponseError code msg) =
--    object ["success"    .= False
--           ,"error_code" .= code
--           ,"message"    .= msg
--           ]
--  toJSON (ResponseBoard sk pk ca) =
--    object ["success" .= True
--           ,"type"    .= ("board" :: String)
--           ,"content" .= object ["secret_key" .= sk
--                                ,"public_key" .= pk
--                                ,"caption"    .= ca
--                                ]
--           ]
--  toJSON (ResponseReset) =
--    object ["success" .= True
--           ,"type"    .= ("reset" :: String)
--           ,"content" .= ("ok" :: String)
--           ]

