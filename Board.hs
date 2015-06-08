{-# LANGUAGE OverloadedStrings #-}
module Board (
  Caption,
  ReporterKey,
  Aha,
  TotalAha,
  Board,
  Error(..),
  new,
  caption,
  viewerConnections,
  addViewerConnection,
  delViewerConnection,
  aha,
  reset,
  hasReporter,
  addReporter,
  allReporterConnections,
  reporterConnections,
  addReporterConnection,
  delReporterConnection,
  reporterAha,
  incrementReporterAha,
  ) where

import qualified Network.WebSockets as WS
import qualified Data.Map.Strict as Map
import Data.Maybe(isJust,catMaybes)

import Data.Aeson.Types(ToJSON,FromJSON,Value(Object),toJSON,parseJSON,object,(.=),(.:))
import Data.Functor ((<$>))
import Control.Applicative (pure,(<*>))
import Control.Monad(mzero)

import Data.List(concatMap)
import qualified AutoMap as AutoMap

type ConnectionKey = AutoMap.Key
type Caption = String
type ReporterKey = String
type Aha = Int
type TotalAha = Int

data ReporterImp = ReporterImp {
  dReporterConnections :: AutoMap.AutoMap WS.Connection,
  dReporterAha :: Aha
  }
                
data BoardImp = BoardImp {
  dBoardCaption :: Caption,
  dBoardViewerConnections :: AutoMap.AutoMap WS.Connection,
  dBoardReporters :: Map.Map ReporterKey ReporterImp
  }

newtype Board = Board BoardImp

data Error = CaptionInvalid
           | ReporterKeyDuplicated
           | ReporterNotFound
           deriving(Show)




new :: Caption -> Either Error Board
new c
  | not $ isValidCaption c = Left CaptionInvalid
  | otherwise              = Right $ Board $ BoardImp c AutoMap.empty Map.empty
  where
    isValidCaption cand = 0 < length cand && length cand <= 20


caption :: Board -> Caption
caption (Board bi) = dBoardCaption bi


viewerConnections :: Board -> [WS.Connection]
viewerConnections (Board bi) = AutoMap.elems $ dBoardViewerConnections bi


addViewerConnection :: Board -> WS.Connection -> (Board,ConnectionKey)
addViewerConnection (Board bi) conn =
  (Board $ bi { dBoardViewerConnections = m' }, k') 
  where
    (m',k') = AutoMap.insert conn (dBoardViewerConnections bi)


delViewerConnection :: Board -> ConnectionKey -> Board
delViewerConnection (Board bi) k =
  Board $ bi { dBoardViewerConnections = m' } 
  where
    m' = AutoMap.delete k (dBoardViewerConnections bi)




aha :: Board -> TotalAha
aha (Board bi) = Map.foldl' (\acc r -> acc + (dReporterAha r)) 0 (dBoardReporters bi)


reset :: Board -> Board
reset (Board bi) = Board $ bi { dBoardReporters = rs' }
  where
    rs' = Map.map (\r -> r { dReporterAha = 0 }) (dBoardReporters bi)




reporter' :: Board -> ReporterKey -> Either Error ReporterImp
reporter' (Board bi) rk = case Map.lookup rk (dBoardReporters bi) of
  Nothing -> Left ReporterNotFound
  Just r -> Right r


insertReporter' :: Board -> ReporterKey -> ReporterImp -> Board
insertReporter' (Board bi) rk r = Board $ bi { dBoardReporters = rs' }
  where
    rs' = Map.insert rk r (dBoardReporters bi)


updateReporter' :: Board -> ReporterKey -> (ReporterImp -> ReporterImp) -> Either Error Board
updateReporter' b rk f = 
  (\r -> insertReporter' b rk (f r)) <$> reporter' b rk 




hasReporter :: Board -> ReporterKey -> Bool
hasReporter (Board bi) rk = Map.member rk (dBoardReporters bi)


addReporter :: Board -> ReporterKey -> Either Error Board
addReporter b rk = case hasReporter b rk of
  True  -> Left ReporterKeyDuplicated
  False -> Right $ insertReporter' b rk (ReporterImp AutoMap.empty 0)






allReporterConnections :: Board -> [WS.Connection]
allReporterConnections (Board bi) = concatMap
                           ( AutoMap.elems . dReporterConnections ) $ Map.elems (dBoardReporters bi)

reporterConnections :: Board -> ReporterKey -> Either Error [WS.Connection]
reporterConnections b rk = (AutoMap.elems . dReporterConnections) <$> reporter' b rk


addReporterConnection :: Board -> ReporterKey -> WS.Connection -> Either Error (Board, ConnectionKey)
addReporterConnection b rk conn = do
  r <- reporter' b rk
  let (m',k') = AutoMap.insert conn (dReporterConnections r)
  let b' = insertReporter' b rk (r { dReporterConnections = m' })
  return (b',k')



delReporterConnection :: Board -> ReporterKey -> ConnectionKey -> Either Error Board
delReporterConnection b rk ck = 
  updateReporter' b rk (\r -> r { dReporterConnections = AutoMap.delete ck (dReporterConnections r) })




reporterAha :: Board -> ReporterKey -> Either Error Aha
reporterAha b rk = dReporterAha <$> reporter' b rk


incrementReporterAha :: Board -> ReporterKey -> Either Error (Board, Aha, TotalAha)
incrementReporterAha b rk = do
  b' <- updateReporter' b rk (\r -> r { dReporterAha = (dReporterAha r) +1 })
  aha' <- reporterAha b' rk
  return (b', aha', aha b')




instance ToJSON ReporterImp where
  toJSON (ReporterImp _ aha) =
    object ["aha" .= aha]

instance FromJSON ReporterImp where
  parseJSON (Object v) = ReporterImp <$> pure AutoMap.empty <*> v .: "aha"
  parseJSON _ = mzero


instance ToJSON BoardImp where
  toJSON (BoardImp caption _ reporters) =
    object ["caption"   .= caption
           ,"reporters" .= reporters
           ]
    
instance FromJSON BoardImp where
  parseJSON (Object v) = BoardImp <$> v .: "caption" <*> pure AutoMap.empty <*> v .: "reporters"
  parseJSON _ = mzero
    

instance ToJSON Board where
  toJSON (Board bi) = toJSON bi

instance FromJSON Board where
  parseJSON o = Board <$> parseJSON o



