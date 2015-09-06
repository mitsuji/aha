{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module JSON (
   ReporterJO (..)
  ,BoardJO (..)
  ,ServerJO (..)
  ,reporterToJO
  ,reporterFromJO
  ,boardToJO
  ,boardFromJO
  ,serverToJO
  ,serverFromJO
  ) where


import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import qualified Data.Map as Map
import Data.Aeson.Types (ToJSON,FromJSON,Value(Object),toJSON,parseJSON,object,(.=),(.:))
import qualified Control.Concurrent.STM as STM

import Server




data ReporterJO = ReporterJO
  { jReporterKey  :: ReporterKey
  , jReporterAha  :: Int
  }
  deriving (Show)

instance ToJSON ReporterJO where
  toJSON ReporterJO{..} =
    object ["key" .= jReporterKey
           ,"aha" .= jReporterAha
           ]

instance FromJSON ReporterJO where
  parseJSON (Object v) = ReporterJO
                         <$> v .: "key"
                         <*> v .: "aha"
  parseJSON _ = mzero



data BoardJO = BoardJO
  { jBoardSecretKey :: BoardSecretKey
  , jBoardPublicKey :: BoardPublicKey
  , jBoardCaption   :: BoardCaption
  , jBoardAha       :: Int
  , jBoardReporters :: Map.Map ReporterKey ReporterJO
  }
  deriving (Show)

instance ToJSON BoardJO where
  toJSON BoardJO{..} =
    object ["secretKey" .= jBoardSecretKey
           ,"publicKey" .= jBoardPublicKey
           ,"caption"   .= jBoardCaption
           ,"aha"       .= jBoardAha
           ,"reporters" .= jBoardReporters
           ]

instance FromJSON BoardJO where
  parseJSON (Object v) = BoardJO
                         <$> v .: "secretKey"
                         <*> v .: "publicKey"
                         <*> v .: "caption"
                         <*> v .: "aha"
                         <*> v .: "reporters"
  parseJSON _ = mzero



data ServerJO = ServerJO
  { jServerBoards    :: Map.Map BoardPublicKey BoardJO
  , jServerBoardKeys :: Map.Map BoardSecretKey BoardPublicKey
  }
  deriving (Show)

instance ToJSON ServerJO where
  toJSON ServerJO{..} =
    object ["boards"    .= jServerBoards
           ,"boardKeys" .= jServerBoardKeys
           ]

instance FromJSON ServerJO where
  parseJSON (Object v) = ServerJO
                         <$> v .: "boards"
                         <*> v .: "boardKeys"
  parseJSON _ = mzero



reporterToJO :: Reporter -> STM.STM ReporterJO
reporterToJO Reporter{..} = do
  aha <- STM.readTVar reporterAha
  return ReporterJO { jReporterKey = reporterKey
                    , jReporterAha = aha
                    }

reporterFromJO :: ReporterJO -> STM.STM Reporter
reporterFromJO ReporterJO{..} = do
  aha   <- STM.newTVar jReporterAha
  chan  <- STM.newBroadcastTChan 
  return Reporter { reporterKey  = jReporterKey
                  , reporterAha  = aha
                  , reporterChan = chan
                  }
  



boardToJO :: Board -> STM.STM BoardJO
boardToJO Board{..} = do
  aha        <- STM.readTVar boardAha
  reporters  <- STM.readTVar boardReporters
  reporters' <- reportersToJOs reporters
  return BoardJO { jBoardSecretKey = boardSecretKey
                 , jBoardPublicKey = boardPublicKey
                 , jBoardCaption   = boardCaption
                 , jBoardAha       = aha
                 , jBoardReporters = reporters'
                 }
    where
      reporterToJO' :: (ReporterKey,Reporter) -> STM.STM (ReporterKey,ReporterJO)
      reporterToJO' (k,i) = do
        i' <- reporterToJO i
        return (k,i')

      reportersToJOs :: Map.Map ReporterKey Reporter -> STM.STM (Map.Map ReporterKey ReporterJO)
      reportersToJOs reporters = do
        reporters' <- mapM reporterToJO' $ Map.toList reporters
        return $ Map.fromList reporters'
      

boardFromJO :: BoardJO -> STM.STM Board
boardFromJO BoardJO{..} = do
  aha        <- STM.newTVar jBoardAha
  reporters  <- reportersFromJOs jBoardReporters
  reporters' <- STM.newTVar reporters
  chan       <- STM.newBroadcastTChan 
  return Board { boardSecretKey = jBoardSecretKey
               , boardPublicKey = jBoardPublicKey
               , boardCaption   = jBoardCaption
               , boardAha       = aha
               , boardReporters = reporters'
               , boardChan      = chan
               }

    where
      reporterFromJO' :: (ReporterKey,ReporterJO) -> STM.STM (ReporterKey,Reporter)
      reporterFromJO' (k,i) = do
        i' <- reporterFromJO i
        return (k,i')

      reportersFromJOs :: Map.Map ReporterKey ReporterJO -> STM.STM (Map.Map ReporterKey Reporter)
      reportersFromJOs reporters = do
        reporters' <- mapM reporterFromJO' $ Map.toList reporters
        return $ Map.fromList reporters'



serverToJO :: Server -> IO ServerJO
serverToJO Server{..} = do

  -- SecretKeyとPublicKeyが矛盾しないように
  (keys,boards) <- STM.atomically $
                   (,) <$> STM.readTVar serverBoardKeys <*> STM.readTVar serverBoards

  boards' <- boardsToJOs boards
  return ServerJO { jServerBoards    = boards'
                  , jServerBoardKeys = keys
                  }
    where
      boardToJO' :: (BoardPublicKey,Board) -> IO (BoardPublicKey,BoardJO)
      boardToJO' (k,i) = do
        i' <- STM.atomically $ boardToJO i -- BoardとReporterが矛盾しないように
        return (k,i')

      boardsToJOs :: Map.Map BoardPublicKey Board -> IO (Map.Map BoardPublicKey BoardJO)
      boardsToJOs boards = do
        boards' <- mapM boardToJO' $ Map.toList boards
        return $ Map.fromList boards'


serverFromJO :: ServerJO -> STM.STM Server
serverFromJO ServerJO{..} = do
  boards  <- boardsFromJOs jServerBoards
  boards' <- STM.newTVar boards
  keys    <- STM.newTVar jServerBoardKeys
  return Server { serverBoards = boards'
                , serverBoardKeys = keys
                }
    where
      boardFromJO' :: (BoardPublicKey,BoardJO) -> STM.STM (BoardPublicKey,Board)
      boardFromJO' (k,i) = do
        i' <- boardFromJO i
        return (k,i')

      boardsFromJOs :: Map.Map BoardPublicKey BoardJO -> STM.STM (Map.Map BoardPublicKey Board)
      boardsFromJOs boards = do
        boards' <- mapM boardFromJO' $ Map.toList boards
        return $ Map.fromList boards'

