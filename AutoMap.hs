module AutoMap (
  Key,
  AutoMap,
  empty,
  insert,
  delete,
  elems,
  ) where

import qualified Data.IntMap.Strict as IntMap

type Key = IntMap.Key
type AutoMapImp a = (IntMap.IntMap a, Key)
newtype AutoMap a = AutoMap (AutoMapImp a)


empty :: AutoMap a
empty = AutoMap (IntMap.empty, 0)


insert :: a -> AutoMap a -> (AutoMap a, Key)
insert i (AutoMap (m,ck)) = (AutoMap(IntMap.insert ck' i m, ck'), ck')
  where
    ck' = ck +1


delete :: Key -> AutoMap a -> AutoMap a
delete k (AutoMap (m,ck)) = AutoMap(IntMap.delete k m,ck)


elems :: AutoMap a -> [a]
elems (AutoMap (m,_)) = IntMap.elems m



test = do
  putStrLn $ show (elems m1, k1)
  putStrLn $ show (elems m2, k2)
  putStrLn $ show (elems m3, k3)
  putStrLn $ show (elems m4, k4)
  putStrLn $ show $ elems m5
  putStrLn $ show (elems m6, k6)
  where
    (m1,k1) = insert "abcd" empty
    (m2,k2) = insert "efgh" m1
    (m3,k3) = insert "ijkl" m2
    (m4,k4) = insert "mnop" m3
    m5 = delete k2 m4
    (m6,k6) = insert "qrxy" m5

