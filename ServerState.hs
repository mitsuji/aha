module ServerState (
  BoardSecretKey,
  BoardPublicKey,
  ServerState,
  new,
  addBoard,
  boardFromPublicKey,
  publicKeyFromSecretKey
  ) where

import qualified Data.Map.Strict as Map
import Control.Concurrent (MVar)

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

addBoard :: ServerState -> BoardSecretKey -> BoardPublicKey -> MVar Board.Board -> ServerState
addBoard ss bsk bpk vb = ServerState $ ServerStateImp secrets' boards'
  where
    secrets' = Map.insert bsk bpk (secrets ssi) -- [TODO] duplication check
    boards' = Map.insert bpk vb (boards ssi) -- [TODO] duplication check    
    ssi = unServerState ss


boardFromPublicKey :: ServerState -> BoardPublicKey -> Maybe (MVar Board.Board)
boardFromPublicKey ss bpk = Map.lookup bpk (boards $ unServerState ss)


publicKeyFromSecretKey :: ServerState -> BoardSecretKey -> Maybe BoardPublicKey
publicKeyFromSecretKey ss bsk = Map.lookup bsk (secrets $ unServerState ss)
