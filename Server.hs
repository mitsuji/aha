{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Server (
   ReporterKey
  ,Reporter (..)
  ,newReporter
  ,BoardSecretKey
  ,BoardPublicKey
  ,BoardCaption
  ,Board (..)
  ,newBoard
  ,Server (..)
  ,newServer
  ,Response (..)
  ,Message (..)
  ,Error (..)
  ,addBoardIO
  ,getBoardFromPublicKey
  ,getBoardFromSecretKey
  ,resetBoard
  ,addReporterIO
  ,getReporter
  ,aha
  ) where


import Data.Data (Data,toConstr)
import Control.Applicative ((<$>),(<*>))
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID
import Data.Aeson.Types (ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async (race)




type ReporterKey = String

data Reporter = Reporter
  { reporterKey  :: ReporterKey
  , reporterAha  :: STM.TVar Int
  , reporterChan :: STM.TChan Message
  }

newReporter :: ReporterKey -> STM.STM Reporter
newReporter rk = do
  aha   <- STM.newTVar 0
  chan  <- STM.newBroadcastTChan 
  return Reporter { reporterKey  = rk
                  , reporterAha  = aha
                  , reporterChan = chan
                  }


type BoardSecretKey = String
type BoardPublicKey = String
type BoardCaption   = String

data Board = Board
  { boardSecretKey :: BoardSecretKey
  , boardPublicKey :: BoardPublicKey
  , boardCaption   :: BoardCaption
  , boardAha       :: STM.TVar Int
  , boardReporters :: STM.TVar (Map.Map ReporterKey Reporter)
  , boardChan      :: STM.TChan Message
  }

newBoard :: BoardSecretKey -> BoardPublicKey -> BoardCaption -> STM.STM Board
newBoard bsk bpk caption = do
  aha       <- STM.newTVar 0
  reporters <- STM.newTVar Map.empty
  chan      <- STM.newBroadcastTChan 
  return Board { boardSecretKey = bsk
               , boardPublicKey = bpk
               , boardCaption   = caption
               , boardAha       = aha
               , boardReporters = reporters
               , boardChan      = chan
               }

  
data Server = Server
  { serverBoards    :: STM.TVar (Map.Map BoardPublicKey Board)
  , serverBoardKeys :: STM.TVar (Map.Map BoardSecretKey BoardPublicKey)
  }

newServer :: IO Server
newServer = do
  boards <- STM.newTVarIO Map.empty
  keys   <- STM.newTVarIO Map.empty
  return Server { serverBoards = boards, serverBoardKeys = keys }
  
  


data Response = ResponseBoard BoardSecretKey BoardPublicKey BoardCaption
              | ResponseReset
              deriving (Show)

instance ToJSON Response where
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



data Message = MessageBoard BoardPublicKey BoardCaption
             | MessageReporter ReporterKey BoardPublicKey BoardCaption
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




data Error = BoardSecretKeyInvalid
           | BoardSecretKeyDuplicated
           | BoardSecretKeyNotSpecified
           | BoardPublicKeyInvalid
           | BoardPublicKeyDuplicated
           | BoardPublicKeyNotSpecified
           | BoardCaptionInvalid
           | BoardCaptionNotSpecified
           | BoardFromSecretKeyNotFound
           | BoardFromPublicKeyNotFound
           | ReporterKeyInvalid
           | ReporterKeyDuplicated
           | ReporterKeyNotSpecified       -- unused
           | ReporterFromSecretKeyNotFound -- unused
           deriving (Show,Typeable,Data)


instance ToJSON Error where
  toJSON (e) = simpleErrorJSON e

simpleErrorJSON :: Error -> AE.Value
simpleErrorJSON e = object ["success" .= False
                             ,"type"    .= (show $ toConstr e)
                             ]





addBoardIO :: Server -> BoardPublicKey -> BoardCaption -> IO (Either Error Board)
addBoardIO server bpk caption
  | not $ isValidPublicKey bpk   = return $ Left BoardPublicKeyInvalid
  | not $ isValidCaption caption = return $ Left BoardCaptionInvalid
  | otherwise = do
    mubsk <- nextUUID
    case mubsk of
      Nothing   -> return $ Left BoardSecretKeyInvalid
      Just ubsk -> STM.atomically $
                   addBoard server (UUID.toString ubsk) bpk caption
  where
    isValidPublicKey cand = (all (\c -> elem c ("abcdefghijklmnopqrstuvwxyz0123456789" :: String) ) cand)
                            && (0 < length cand && length cand <= 20)
    isValidCaption cand = 0 < length cand && length cand <= 20




addBoard :: Server -> BoardSecretKey -> BoardPublicKey -> BoardCaption -> STM.STM (Either Error Board)
addBoard Server{..} bsk bpk caption = do

  keys   <- STM.readTVar serverBoardKeys
  boards <- STM.readTVar serverBoards

  case () of
    _ | Map.member bsk keys   -> return $ Left BoardSecretKeyDuplicated
      | Map.member bpk boards -> return $ Left BoardPublicKeyDuplicated
      | otherwise -> do
        board <- newBoard bsk bpk caption
        STM.writeTVar serverBoardKeys $ Map.insert bsk bpk keys
        STM.writeTVar serverBoards    $ Map.insert bpk board boards
        return $ Right board


getBoardFromPublicKey :: Server -> BoardPublicKey -> STM.STM (Maybe Board)
getBoardFromPublicKey Server{..} bpk = 
  (Map.lookup bpk) <$> STM.readTVar serverBoards
  

getBoardFromSecretKey :: Server -> BoardSecretKey -> STM.STM (Maybe Board)
getBoardFromSecretKey server@Server{..} bsk = do
  mbpk <- (Map.lookup bsk) <$> STM.readTVar serverBoardKeys
  case mbpk of
    Nothing  -> return $ Nothing
    Just bpk -> getBoardFromPublicKey server bpk




resetBoard :: Board -> STM.STM ()
resetBoard Board{..} = do
  reporters <- STM.readTVar boardReporters

  -- reset all
  STM.writeTVar boardAha 0
  mapM_ (\r -> STM.writeTVar (reporterAha r) 0) (Map.elems reporters)

  -- reset message
  STM.writeTChan boardChan MessageReset
  



addReporterIO :: Board -> IO (Either Error Reporter)
addReporterIO board = do
  murk <- nextUUID
  case murk of
    Nothing  -> return $ Left ReporterKeyInvalid
    Just urk -> STM.atomically $
                addReporter board (UUID.toString urk)


addReporter :: Board -> ReporterKey -> STM.STM (Either Error Reporter)
addReporter Board{..} rk = do
  reporters <- STM.readTVar boardReporters
  if Map.member rk reporters
    then return $ Left ReporterKeyDuplicated
    else do
      reporter <- newReporter rk
      STM.writeTVar boardReporters $ Map.insert rk reporter reporters
      return $ Right reporter
              

getReporter :: Board -> ReporterKey -> STM.STM (Maybe Reporter)
getReporter Board{..} rk =
  (Map.lookup rk) <$> STM.readTVar boardReporters




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
  


