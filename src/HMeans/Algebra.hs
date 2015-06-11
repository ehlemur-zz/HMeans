{-# LANGUAGE TypeSynonymInstances , FlexibleInstances #-}

module HMeans.Algebra where

import HMeans.Common

import qualified Data.Traversable as T

import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as UV

import Debug.Trace


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

class AbelianGroup a where
    zero :: a
    neg :: a -> a
    plus :: a -> a -> a
    minus :: a -> a -> a

    neg = (zero `minus`)
    minus a = (a `plus`) . neg


class (Ord a, AbelianGroup a) => Vector a where
    mapSq :: a -> a
    over :: a -> Double -> a
    distSq :: a -> a -> Double


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


    
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

instance (AbelianGroup a) => AbelianGroup (Cluster a) where 
    -- zero :: Cluster
    zero = Cluster 0 zero zero ISet.empty ISet.empty

    -- plus :: Cluster -> Cluster -> Cluster
    (Cluster 0 _ _ _ _) `plus` y                        = y
    x                   `plus` (Cluster 0 _ _ _ _)      = x
    (Cluster n1 l1 s1 i1 o1) `plus` (Cluster n2 l2 s2 i2 o2) = seq n1 $ seq n2 $ seq l1 $ seq l2 $ seq s1 $ seq s2 $ seq i1 $ seq i2 $ seq o1 $ seq o2 $ Cluster n l s i o
      where
        n = ((+) $!) n1 $! n2
        l = (plus $!) l1 $! l2
        s = (plus $!) s1 $! s2
        i' = (ISet.union $!) i1 $! i2
        o' = (ISet.union $!) o1 $! o2
        i = (ISet.difference $!) i' $! o'
        o = (ISet.difference $!) o' $! i'

    neg (Cluster n l s i o) = Cluster (negate n) (neg l) (neg s) o i

--------------------------------------------------------------------------------

instance (AbelianGroup a) => AbelianGroup (Partition a) where 
    zero = Partition IMap.empty

    -- plus (Partition x) | trace (show $ IMap.size x) False = Partition . IMap.unionWith plus x . getPartition
    plus (Partition x) (Partition y) = seq x $ seq y $ Partition $! IMap.unionWith (plus $!) x y

    neg = Partition . IMap.map neg . getPartition

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

instance AbelianGroup DoubleList where
    zero = repeat 0
    plus x = zipWith (+) x

    neg = map (0 -) 

instance Vector DoubleList where
    mapSq = map (^2)
    over a s = map (/ s) a
    distSq x = sum . map (^2) . zipWith (-) x 

--------------------------------------------------------------------------------

instance AbelianGroup (UV.Vector Double) where
    zero = UV.empty
    plus x = UV.zipWith (+) x 
    neg = UV.map (0 -)

instance Vector (UV.Vector Double) where 
    mapSq = UV.map (^2)
    over a s = UV.map (/ s) a
    distSq x = UV.sum . UV.map (^2) . UV.zipWith (-) x

--------------------------------------------------------------------------------

instance AbelianGroup (IMap.IntMap Double) where
    zero = IMap.empty
    plus x = IMap.unionWith (+) x
    neg = IMap.map (0 -)

instance Vector (IMap.IntMap Double) where
    mapSq = IMap.map (^2)
    over a s = IMap.map (/ s) a
    distSq x = IMap.foldl' (+) 0 . IMap.map (^2) . IMap.unionWith (-) x

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
