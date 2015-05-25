{-# LANGUAGE OverloadedStrings #-}
module ServerState (
  BoardSecretKey,
  BoardPublicKey,
  ServerState,
  Error(..),
  new,
  addBoard,
  boardFromPublicKey,
  publicKeyFromSecretKey,
  Item,
  dump,
  restore,
  ) where

import qualified Data.Map.Strict as Map
import Control.Concurrent (MVar,newMVar,readMVar)

import Data.Aeson.Types(ToJSON,FromJSON,Value(Object),toJSON,parseJSON,object,(.=),(.:))
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Monad(mzero)
import Data.Maybe(fromJust)

import Control.Monad(forM,foldM)
import Control.Exception(throwIO)
import System.IO.Error(userError)

import qualified Board as Board




type BoardSecretKey = String
type BoardPublicKey = String

data ServerStateImp = ServerStateImp {
  secrets :: Map.Map BoardSecretKey BoardPublicKey,
  boards :: Map.Map BoardPublicKey (MVar Board.Board)
  }

newtype ServerState = ServerState { unServerState :: ServerStateImp }

data Error = BoardSecretKeyDuplicated
           | BoardPublicKeyDuplicated
           | BoardSecretKeyInvalid
           | BoardPublicKeyInvalid
           deriving(Show)




new :: ServerState
new = ServerState $ ServerStateImp Map.empty Map.empty


addBoard :: ServerState -> BoardSecretKey -> BoardPublicKey -> MVar Board.Board -> Either Error ServerState
addBoard ss bsk bpk vboard
  | not $ isValidSecretKey bsk = Left BoardSecretKeyInvalid
  | not $ isValidPublicKey bpk = Left BoardPublicKeyInvalid
  | otherwise = case Map.member bsk srs of
    True -> Left BoardSecretKeyDuplicated
    False -> case Map.member bpk bds of
      True -> Left BoardPublicKeyDuplicated
      False ->  Right $ ServerState $ ServerStateImp srs' bds'
  where
    isValidSecretKey cand = True -- must be UUID
    isValidPublicKey cand = (all (\c -> elem c "abcdefghijklmnopqrstuvwxyz0123456789") cand)
                            && (0 < length cand && length cand <= 20)
    srs' = Map.insert bsk bpk srs
    bds' = Map.insert bpk vboard bds
    srs = secrets ssi
    bds = boards ssi
    ssi = unServerState ss


boardFromPublicKey :: ServerState -> BoardPublicKey -> Maybe (MVar Board.Board)
boardFromPublicKey ss bpk = Map.lookup bpk (boards $ unServerState ss)


publicKeyFromSecretKey :: ServerState -> BoardSecretKey -> Maybe BoardPublicKey
publicKeyFromSecretKey ss bsk = Map.lookup bsk (secrets $ unServerState ss)




data Item = Item BoardSecretKey BoardPublicKey Board.Board

instance ToJSON Item where
  toJSON (Item sk pk b) =
    object ["secret_key" .= sk
           ,"public_key" .= pk
           ,"board"      .= b
           ]

instance FromJSON Item where
  parseJSON (Object v) = Item <$> v .: "secret_key" <*> v .: "public_key" <*> v.: "board"
  parseJSON _ = mzero


dump :: ServerState -> IO [Item]
dump ss = forM srs $ \(sk, pk) -> do
  board <- readMVar $ fromJust $ Map.lookup pk bds
  return $ Item sk pk board
  where
    srs = Map.toList $ secrets ssi
    bds = boards ssi
    ssi = unServerState ss

restore :: [Item] -> IO ServerState
restore items = foldM restore' ServerState.new items
  where
    restore' :: ServerState -> Item -> IO ServerState
    restore' state (Item bsk bpk board) = do
      vboard <- newMVar board
      case addBoard state bsk bpk vboard of
        Left err -> throwIO $ userError $ show err
        Right state' -> return state'

