module Board (
  ReporterKey,
  Board,
  new,
  connection,
  setConnection,
  closeConnection,
  reporter,
  addReporter,
  reporterConnections,
  setReporterConnection,
  closeReporterConnection,
  reset,
  aha,
  reporterAhaCount,
  boardAhaCount,
  ) where

import qualified Network.WebSockets as WS
import qualified Data.Map.Strict as Map
import Data.Maybe(catMaybes)
import Data.Either(Either(Right,Left))

type ReporterKey = String

data Reporter = Reporter {
  rconn :: Maybe WS.Connection,
  ahaCount :: Int
  }
                
data BoardImp = BoardImp {
  bconn :: Maybe WS.Connection,
  reporters :: Map.Map ReporterKey Reporter
  }

newtype Board = Board { unBoard :: BoardImp }


new :: WS.Connection -> Board
new conn = Board $ BoardImp (Just conn) Map.empty


connection :: Board -> Maybe WS.Connection
connection b = bconn $ unBoard b


setConnection :: Board -> WS.Connection -> Either String Board
setConnection b conn = Right $ case bconn bi of
  Just _ -> error "active session"
  Nothing -> Board $ bi { bconn = Just conn } 
  where
    bi = unBoard b


closeConnection :: Board -> Board
closeConnection b = Board $ (unBoard b) { bconn = Nothing } 


reporter :: Board -> ReporterKey -> Bool
reporter b rk = Map.member rk (reporters $ unBoard b)

addReporter :: Board -> ReporterKey -> WS.Connection -> Board
addReporter b rk conn = Board $ bi { reporters = reporters' }
  where
    reporters' = Map.insert rk (newReporter rconn) (reporters bi)
    newReporter rconn = Reporter (Just conn) 0
    bi = unBoard b
    

reporterConnections :: Board -> [WS.Connection]
reporterConnections b = catMaybes
                        $ map (\(Reporter rconn _) -> rconn)
                        $ Map.elems (reporters $ unBoard b)


setReporterConnection :: Board -> ReporterKey -> WS.Connection -> Either String Board
setReporterConnection b rk conn = Right $ Board $ bi { reporters = reporters' }
  where
    reporter = case Map.lookup rk (reporters bi) of
      Just r -> r
      Nothing -> error "invalid reporter key"
    reporter' = case rconn reporter of
      Just _ -> error "active session"
      Nothing -> reporter { rconn = Just conn }
    reporters' = Map.insert rk reporter' (reporters bi)
    bi = unBoard b


closeReporterConnection :: Board -> ReporterKey -> Either String Board
closeReporterConnection b rk = Right $Board $ bi { reporters = reporters' }
  where
    reporter = case Map.lookup rk (reporters bi) of
      Just r -> r
      Nothing -> error "invalid reporter key"
    reporter' = reporter { rconn = Nothing }
    reporters' = Map.insert rk reporter' (reporters bi)
    bi = unBoard b


reset :: Board -> Board
reset b = Board $ bi { reporters = reporters' }
  where
    reporters' = Map.map (\reporter -> reporter { ahaCount = 0 }) (reporters bi)
    bi = unBoard b


aha :: Board -> ReporterKey -> Either String (Board, Int, Int)
aha b rk = Right $( Board $ bi { reporters = reporters' }, ahaCount', total')
  where
    reporter = case Map.lookup rk (reporters bi) of
      Just r -> r
      Nothing -> error "invalid reporter key"
    ahaCount' = (ahaCount reporter) +1
    reporter' = reporter { ahaCount = ahaCount' }
    reporters' = Map.insert rk reporter' (reporters bi)
    total' = Map.foldl' (\acc (Reporter _ count) -> acc + count) 0 reporters'
    bi = unBoard b


reporterAhaCount :: Board -> ReporterKey -> Either String Int
reporterAhaCount b rk = Right $ ahaCount reporter
  where
    reporter = case Map.lookup rk (reporters bi) of
      Just r -> r
      Nothing -> error "invalid reporter key"
    bi = unBoard b


boardAhaCount :: Board -> Int
boardAhaCount b = Map.foldl' (\acc (Reporter _ count) -> acc + count) 0 (reporters $ unBoard b)

