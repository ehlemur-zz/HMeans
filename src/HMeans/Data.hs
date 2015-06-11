{-# LANGUAGE TypeSynonymInstances , FlexibleInstances #-}

module HMeans.Data where

import HMeans.Common
import HMeans.Algebra


import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map

import Debug.Trace



class Data a where
    getId :: a b -> Int
    toCluster :: a b -> Cluster b


instance Data BasicData where
    getId = snd . getData
    toCluster (BasicData (d, i)) = Cluster 1 d d (ISet.singleton i) ISet.empty


toBasicData :: [d] -> [BasicData d]
toBasicData a = map BasicData . flip zip [1..] $ a
