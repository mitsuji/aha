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
  connection,
  setConnection,
  closeConnection,
  aha,
  reset,
  reporterConnections,
  hasReporter,
  addReporter,
  reporterConnection,
  setReporterConnection,
  closeReporterConnection,
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




type Caption = String
type ReporterKey = String
type Aha = Int
type TotalAha = Int

data Reporter = Reporter {
  dReporterConnection :: Maybe WS.Connection,
  dReporterAha :: Aha
  }
                
data BoardImp = BoardImp {
  dBoardCaption :: Caption,
  dBoardConnection :: Maybe WS.Connection,
  dBoardReporters :: Map.Map ReporterKey Reporter
  }

newtype Board = Board { unBoard :: BoardImp }

data Error = CaptionInvalid
           | ReporterKeyDuplicated
           | ReporterNotFound
           | ActiveConnectionExists
           deriving(Show)




new :: Caption -> Either Error Board
new c
  | not $ isValidCaption c = Left CaptionInvalid
  | otherwise              = Right $ Board $ BoardImp c Nothing Map.empty
  where
    isValidCaption cand = 0 < length cand && length cand <= 20


caption :: Board -> Caption
caption b = dBoardCaption $ unBoard b 


connection :: Board -> Maybe WS.Connection
connection b = dBoardConnection $ unBoard b


setConnection :: Board -> WS.Connection -> Either Error Board
setConnection b conn = case isJust $ connection b of
  True  -> Left ActiveConnectionExists
  False -> Right $ Board $ (unBoard b) { dBoardConnection = Just conn } 


closeConnection :: Board -> Board
closeConnection b = Board $ (unBoard b) { dBoardConnection = Nothing } 


aha :: Board -> TotalAha
aha b = Map.foldl' (\acc r -> acc + (dReporterAha r)) 0 (dBoardReporters $ unBoard b)


reset :: Board -> Board
reset b = Board $ bi { dBoardReporters = rs' }
  where
    rs' = Map.map (\r -> r { dReporterAha = 0 }) (dBoardReporters bi)
    bi = unBoard b


reporterConnections :: Board -> [WS.Connection]
reporterConnections b = catMaybes
                        $ map dReporterConnection $ Map.elems (dBoardReporters $ unBoard b)




reporter' :: Board -> ReporterKey -> Either Error Reporter
reporter' b rk = case Map.lookup rk (dBoardReporters $ unBoard b) of
  Nothing -> Left ReporterNotFound
  Just r -> Right r


insertReporter' :: Board -> ReporterKey -> Reporter -> Board
insertReporter' b rk r = Board $ bi { dBoardReporters = rs' }
  where
    rs' = Map.insert rk r (dBoardReporters bi)
    bi = unBoard b


updateReporter' :: Board -> ReporterKey -> (Reporter -> Reporter) -> Either Error Board
updateReporter' b rk f = 
  (\r -> insertReporter' b rk (f r)) <$> reporter' b rk 




hasReporter :: Board -> ReporterKey -> Bool
hasReporter b rk = Map.member rk (dBoardReporters $ unBoard b)


addReporter :: Board -> ReporterKey -> Either Error Board
addReporter b rk = case hasReporter b rk of
  True  -> Left ReporterKeyDuplicated
  False -> Right $ insertReporter' b rk (Reporter Nothing 0)


reporterConnection :: Board -> ReporterKey -> Either Error (Maybe WS.Connection)
reporterConnection b rk = dReporterConnection <$> reporter' b rk

  
setReporterConnection :: Board -> ReporterKey -> WS.Connection -> Either Error Board
setReporterConnection b rk conn = do
  hasConn <- isJust <$> reporterConnection b rk
  case hasConn of
    True  -> Left ActiveConnectionExists
    False -> updateReporter' b rk (\r -> r { dReporterConnection = Just conn })


closeReporterConnection :: Board -> ReporterKey -> Either Error Board
closeReporterConnection b rk =
  updateReporter' b rk (\r -> r { dReporterConnection = Nothing })
         

reporterAha :: Board -> ReporterKey -> Either Error Aha
reporterAha b rk = dReporterAha <$> reporter' b rk


incrementReporterAha :: Board -> ReporterKey -> Either Error (Board, Aha, TotalAha)
incrementReporterAha b rk = do
  b'   <- updateReporter' b rk (\r -> r { dReporterAha = (dReporterAha r) +1 })
  aha' <- reporterAha b' rk
  return (b', aha', aha b')




instance ToJSON Reporter where
  toJSON (Reporter _ aha) =
    object ["aha" .= aha]

instance FromJSON Reporter where
  parseJSON (Object v) = Reporter <$> pure Nothing <*> v .: "aha"
  parseJSON _ = mzero


instance ToJSON BoardImp where
  toJSON (BoardImp caption _ reporters) =
    object ["caption"   .= caption
           ,"reporters" .= reporters
           ]
    
instance FromJSON BoardImp where
  parseJSON (Object v) = BoardImp <$> v .: "caption" <*> pure Nothing <*> v .: "reporters"
  parseJSON _ = mzero
    

instance ToJSON Board where
  toJSON (Board bi) = toJSON bi

instance FromJSON Board where
  parseJSON o = Board <$> parseJSON o



