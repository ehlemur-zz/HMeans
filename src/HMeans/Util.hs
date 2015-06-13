{-# LANGUAGE TypeSynonymInstances , FlexibleInstances #-}

module HMeans.Util where

import HMeans.Common
import HMeans.Algebra

import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map


getCentroid :: Vector v => Cluster v -> v
getCentroid (Cluster n xs _ _ _) = xs `over` (fromIntegral n)

clDist :: Vector v => Cluster v -> Cluster v -> Double
clDist c d = distSq (getCentroid c) (getCentroid d)

closestCluster :: Vector v => IMap.IntMap (Cluster v) -> Cluster v -> Int
closestCluster p c = snd $ IMap.foldlWithKey' f (read "Infinity", undefined) $ IMap.map (\c' -> clDist c c') p
  where
    f a@(d, _) k d' = if d < d' 
                        then a
                        else (d', k)

toNum :: (Integral a, Num b) => a -> b
toNum = fromInteger . toInteger

wrap :: Monad m => (a -> b) -> a -> m b
wrap f a = return $ f a
