{-# LANGUAGE TypeSynonymInstances , FlexibleInstances #-}

module HMeans.Preprocess where

import HMeans.Util
import HMeans.Data
import HMeans.Common
import HMeans.Algebra


import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map

import Debug.Trace
import System.Random
import System.Random.Shuffle



randomInitialize :: (Vector v, Data d) => Params -> StdGen -> [d v] -> Partition v
randomInitialize params gen0 = Partition . IMap.fromAscList . zip [1..] . rInitialize (generateRandom $ gen0) . map toCluster
  where
    treshold = ((toNum $ nMicroClusters params) :: Double) / ((toNum $ nDataPoints params) :: Double) 

    rInitialize :: [Bool] -> [a] -> [a]
    rInitialize _ [] = []
    rInitialize (x:xs) (y:ys) = if x then y : rInitialize xs ys else rInitialize xs ys

    generateRandom :: (RandomGen g) => g -> [Bool]
    generateRandom gen = let (x, gen') = randomR (0.0, 1.0) gen in (x < treshold) : generateRandom gen'



initialize :: (Vector v, Data d) => Params -> [d v] -> Partition v
initialize params = Partition 
                    . IMap.fromAscList 
                    . (take $ nMicroClusters params) 
                    . (zip [1..]) 
                    . (map toCluster)
