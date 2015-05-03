module ServerState (
  BoardSecretKey,
  BoardPublicKey,
  ServerState,
  new,
  addBoard,
  boardFromPublicKey,
  publicKeyFromSecretKey,
  dump,
  ) where

import qualified Data.Map.Strict as Map
import Control.Concurrent (MVar)
import Data.Maybe(fromJust)

import qualified Board as Board




type BoardSecretKey = String
type BoardPublicKey = String

data ServerStateImp = ServerStateImp {
  secrets :: Map.Map BoardSecretKey BoardPublicKey,
  boards :: Map.Map BoardPublicKey (MVar Board.Board)
  }

newtype ServerState = ServerState { unServerState :: ServerStateImp }




new :: ServerState
new = ServerState $ ServerStateImp Map.empty Map.empty

addBoard :: ServerState -> BoardSecretKey -> BoardPublicKey -> MVar Board.Board -> Either String ServerState
addBoard ss bsk bpk vb =
  case Map.member bsk srs of -- duplication check
    True -> Left "BaordSecretKey duplicated"
    False -> case Map.member bpk bds of -- duplication check
      True -> Left "BoardPublicKey duplicated"
      False ->  Right $ ServerState $ ServerStateImp srs' bds'
  where
    srs' = Map.insert bsk bpk srs
    bds' = Map.insert bpk vb bds
    srs = secrets ssi
    bds = boards ssi
    ssi = unServerState ss


boardFromPublicKey :: ServerState -> BoardPublicKey -> Maybe (MVar Board.Board)
boardFromPublicKey ss bpk = Map.lookup bpk (boards $ unServerState ss)


publicKeyFromSecretKey :: ServerState -> BoardSecretKey -> Maybe BoardPublicKey
publicKeyFromSecretKey ss bsk = Map.lookup bsk (secrets $ unServerState ss)




dump :: ServerState -> [(BoardSecretKey, BoardPublicKey, MVar Board.Board)]
dump ss = map ( \(sk, pk) -> (sk, pk, fromJust $ Map.lookup pk bds ) ) $ Map.toList srs
  where
    srs = secrets ssi
    bds = boards ssi
    ssi = unServerState ss
