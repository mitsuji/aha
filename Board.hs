{-# LANGUAGE OverloadedStrings #-}
module Board (
  Caption,
  ReporterKey,
  Board,
  Error,
  new,
  caption,
  connection,
  setConnection,
  closeConnection,
  hasReporter,
  addReporter,
  reporterConnections,
  setReporterConnection,
  closeReporterConnection,
  reset,
  aha,
  reporterAhaCount,
  totalAhaCount,
  ) where

import qualified Network.WebSockets as WS
import qualified Data.Map.Strict as Map
import Data.Maybe(catMaybes)

import Data.Aeson.Types(ToJSON,FromJSON,Value(Object),toJSON,parseJSON,object,(.=),(.:))
import Data.Functor ((<$>))
import Control.Applicative (pure,(<*>))
import Control.Monad(mzero)




type Caption = String
type ReporterKey = String

data Reporter = Reporter {
  rconn :: Maybe WS.Connection,
  ahaCount :: Int
  }
                
data BoardImp = BoardImp {
  bcaption :: Caption,
  bconn :: Maybe WS.Connection,
  reporters :: Map.Map ReporterKey Reporter
  }

newtype Board = Board { unBoard :: BoardImp }

data Error = ActiveConnectionExists
           | ReporterNotFound
           deriving(Show)




new :: String -> Board
new capt = Board $ BoardImp capt Nothing Map.empty


caption :: Board -> Caption
caption b = bcaption $ unBoard b 


connection :: Board -> Maybe WS.Connection
connection b = bconn $ unBoard b


setConnection :: Board -> WS.Connection -> Either Error Board
setConnection b conn = case bconn bi of
  Just _ -> Left ActiveConnectionExists
  Nothing -> Right $ Board $ bi { bconn = Just conn } 
  where
    bi = unBoard b


closeConnection :: Board -> Board
closeConnection b = Board $ (unBoard b) { bconn = Nothing } 


hasReporter :: Board -> ReporterKey -> Bool
hasReporter b rk = Map.member rk $ reporters $ unBoard b


addReporter :: Board -> ReporterKey -> Board
addReporter b rk = Board $ bi { reporters = rs' }
  where
    rs' = Map.insert rk r' (reporters bi)
    r' = Reporter Nothing 0
    bi = unBoard b


reporterConnections :: Board -> [WS.Connection]
reporterConnections b = catMaybes
                        $ map (\(Reporter rconn _) -> rconn)
                        $ Map.elems (reporters $ unBoard b)


setReporterConnection :: Board -> ReporterKey -> WS.Connection -> Either Error Board
setReporterConnection b rk conn = case Map.lookup rk rs of
  Nothing -> Left ReporterNotFound
  Just r -> case rconn r of
    Just _ -> Left ActiveConnectionExists
    Nothing -> Right $ Board $ bi { reporters = rs' r }
  where
    rs' r = Map.insert rk (r { rconn = Just conn }) rs
    rs = reporters bi
    bi = unBoard b


closeReporterConnection :: Board -> ReporterKey -> Either Error Board
closeReporterConnection b rk = case Map.lookup rk rs of
  Nothing -> Left ReporterNotFound
  Just r -> Right $ Board $ bi { reporters = rs' r }
  where
    rs' r = Map.insert rk (r { rconn = Nothing }) rs
    rs = reporters bi
    bi = unBoard b
         

reset :: Board -> Board
reset b = Board $ bi { reporters = rs' }
  where
    rs' = Map.map (\r -> r { ahaCount = 0 }) (reporters bi)
    bi = unBoard b


aha :: Board -> ReporterKey -> Either Error (Board, Int)
aha b rk = case Map.lookup rk rs of
  Nothing -> Left ReporterNotFound
  Just r -> Right $ (Board $ bi { reporters = rs' r }, ac' r)
  where
    rs' r = Map.insert rk r { ahaCount = ac' r } rs
    ac' r = (ahaCount r) +1 -- reporter's count
    rs = reporters bi
    bi = unBoard b


reporterAhaCount :: Board -> ReporterKey -> Maybe Int
reporterAhaCount b rk = case Map.lookup rk (reporters $ unBoard b) of
  Nothing -> Nothing
  Just r -> Just $ ahaCount r


totalAhaCount :: Board -> Int
totalAhaCount b = Map.foldl' (\acc (Reporter _ count) -> acc + count) 0 (reporters $ unBoard b)




instance ToJSON Reporter where
  toJSON (Reporter _ ahaCount) =
    object ["ahaCount" .= ahaCount]

instance FromJSON Reporter where
  parseJSON (Object v) = Reporter <$> (pure Nothing) <*> v .: "ahaCount"
  parseJSON _ = mzero


instance ToJSON BoardImp where
  toJSON (BoardImp caption _ reporters) =
    object ["caption"   .= caption
           ,"reporters" .= reporters
           ]
    
instance FromJSON BoardImp where
  parseJSON (Object v) = BoardImp <$> v .: "caption" <*> (pure Nothing) <*> v .: "reporters"
  parseJSON _ = mzero
    

instance ToJSON Board where
  toJSON (Board bi) = toJSON bi

instance FromJSON Board where
  parseJSON o = Board <$> parseJSON o



