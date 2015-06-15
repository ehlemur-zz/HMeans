{-# LANGUAGE TypeSynonymInstances , FlexibleInstances #-}

module HMeans.KMeans where

import HMeans.Data
import HMeans.Util
import HMeans.Common
import HMeans.Algebra

import qualified Data.Set as Set
import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

import System.Random

import Debug.Trace
import Control.Exception.Base

import Data.List
import Data.Clustering.Hierarchical




runHMeans :: Vector v => Params -> Partition v -> Partition v
runHMeans a b = case (hParams a) of
                  (HierarchicalParams _) -> runHierarchical a b
                  (KMeansParams       _) -> runKMeans a b



partitionToLabelList :: Vector v => Partition v -> [Int]
partitionToLabelList pcls = map snd $ sort $ concat $ map clusterToLabelList $ IMap.assocs $ getPartition pcls
  where
    clusterToLabelList (k, c) = flip zip (repeat k) $ ISet.toAscList $ pointsIn c






runHierarchical :: Vector v => Params -> Partition v -> Partition v
runHierarchical params pcls = Partition $ IMap.fromList $ zip [0..] $ map dendroToCluster $ getDendroList (nClusters params) dendro
  where 
    dendro = dendrogram (linkageType $ hParams params) (IMap.elems $ getPartition pcls) clDist 

    getDendroList :: Ord a => Int -> Dendrogram a -> [Dendrogram a]
    getDendroList k dendro@(Branch d _ _)  = getDendroList' k (1, [dendro])
      where
        getDendroList' k (n, xs) = 
          if n == k
            then xs
            else getDendroList' k (n + 1, xs')
          where
            dendro@(Branch _ a b) = maximum xs
            xs' = filter (/= dendro) xs ++ [a, b]

    dendroToCluster :: Vector v => Dendrogram (Cluster v) -> Cluster v
    dendroToCluster (Leaf x) = x
    dendroToCluster (Branch _ a b) = (plus $!) (dendroToCluster a) (dendroToCluster b)








runKMeansIO :: Vector v => Params -> String -> (String -> IO [BasicData v]) -> IO (Partition v)
runKMeansIO parameters inputFile inputSource = 
  do let k = nClusters parameters
     let prob = 0.9 :: Double 
     let nIters = maxIters $ hParams parameters

     let kmeansStep :: Vector v => String -> (String -> IO [BasicData v]) -> IMap.IntMap (Cluster v) -> IO (IMap.IntMap (Cluster v))
         kmeansStep inputFile inputSource p = 
           do assignments <- inputSource inputFile
                               >>= wrap (map toCluster) 
                               >>= wrap (map $ closestCluster p) 
              inputSource inputFile 
                >>= wrap (map toCluster)
                >>= wrap (zip assignments) 
                >>= wrap (IMap.fromListWith plus)
     
     let kmeans :: Vector v => String -> (String -> IO [BasicData v]) -> Int -> IO (IMap.IntMap (Cluster v)) -> IO (IMap.IntMap (Cluster v))
         kmeans inputFile inputSource 0 pIO = pIO
         kmeans inputFile inputSource n pIO = 
           do p <- pIO
              p' <- kmeansStep inputFile inputSource p
              if p == p' then pIO
              else kmeans inputFile inputSource (n-1) (return p')
           
     let generateRandom :: RandomGen g => g -> [Bool]
         generateRandom gen = let (x, gen') = randomR (0.0, 1.0) gen in (x < prob) : generateRandom gen'

     let randomInitialize :: Vector v => Int -> Int -> [Bool] -> [Cluster v] -> [Cluster v]
         randomInitialize 0 _ _      _          = []
         randomInitialize n m (b:bs) cs@(c:cs') 
           | n == m    = cs
           | otherwise = if b then
                           c : randomInitialize (n-1) (m-1) bs cs'
                         else
                           randomInitialize n (m-1) bs cs'


     randomSeed <- getStdGen

     let initial = inputSource inputFile 
                     >>= wrap (map toCluster)
                     >>= wrap (randomInitialize k (nDataPoints parameters) $ generateRandom randomSeed)
                     >>= wrap (zip [0..]) 
                     >>= wrap IMap.fromAscList    
     
     kmeans inputFile inputSource nIters initial >>= wrap Partition





runKMeans :: Vector v => Params -> Partition v -> Partition v
runKMeans params pcls = Partition $ kmeans nIters (IMap.elems _cls) initial
  where
    k = nClusters params
    _cls = getPartition pcls
    nIters = maxIters $ hParams params
    initial = IMap.fromAscList $ zip [0..] $ take k $ IMap.elems _cls 

    kmeans :: Vector v => Int -> [Cluster v] -> IMap.IntMap (Cluster v) -> IMap.IntMap (Cluster v)
    kmeans 0 _   p = p
    kmeans n cls p = let p' = kmeansStep p cls in 
                     if p == p' 
                       then p
                       else kmeans (n-1) cls p'
                     
    kmeansStep :: Vector v => IMap.IntMap (Cluster v) -> [Cluster v] -> IMap.IntMap (Cluster v)
    kmeansStep p xs = IMap.fromListWith plus $ flip zip xs $ map (closestCluster p) xs
      
{-partition :: (Vector v) => Params -> Partition v -> Partition v
partition params _pcls = Partition $ partition' (nClusters params) _cls distTable
  where 
    _cls = getPartition _pcls
    distTable = Map.fromListWith Set.union [(clDist c d, Set.singleton (i, j)) | (i, c) <- IMap.assocs _cls , (j, d) <- IMap.assocs _cls , i < j]
    
    partition' :: (Vector v) => Int -> IMap.IntMap (Cluster v) -> Map.Map Double (Set.Set (Int, Int)) -> IMap.IntMap (Cluster v)
    partition' n cls dtable {-| trace (show $ IMap.size cls) True -} = 
      if IMap.size cls <= n 
        then cls
        else partition' n cls' dtable' --(trace ((show $ IMap.size cls) ++ " = " ++ (show dtable')) $ dtable')
      where 
        (d, s) = Map.findMin dtable
        (_i, _j) = Set.findMin s

        c1 = -- trace "1" $ 
             cls IMap.! _i
        c2 = -- trace ("2" ++ show (_i, _j)) $
             cls IMap.! _j
        c12 = c1 `plus` c2

        cls' = IMap.insert _i c12 $ IMap.delete _i $ IMap.delete _j cls
        dtable' = addUpdate $ delUpdate dtable


        addUpdate mp = -- trace ((show $ IMap.size cls) ++ " + " ++ (show $ [(clDist c12 c, Set.singleton $ sortedPair (_i, j)) | (j, c) <- IMap.assocs cls' , _i /= j])) $
                       foldr ($) mp $ map (uncurry $ Map.insertWith Set.union) 
                       [(clDist c12 c, Set.singleton $ sortedPair (_i, j)) | (j, c) <- IMap.assocs cls' , _i /= j]

        delUpdate mp = -- trace ((show $ IMap.size cls) ++ " - " ++ (show $ [(d, (_i, _j))] 
                       -- ++ [(clDist c1 c, sortedPair (_i, j)) | (j, c) <- IMap.assocs cls, j /= _i, j /= _j] 
                       -- ++ [(clDist c c2, sortedPair (i, _j)) | (i, c) <- IMap.assocs cls, i /= _i, i /= _j])) $ 
                       -- assert (dtable == (Map.fromListWith Set.union [(clDist c d, Set.singleton (i, j)) | (i, c) <- IMap.assocs cls , (j, d) <- IMap.assocs cls , i < j])) $
                       foldr ($) mp $ map singleUpdate $ [(d, (_i, _j))] 
                       ++ [(clDist c1 c, sortedPair (_i, j)) | (j, c) <- IMap.assocs cls, j /= _i, j /= _j] 
                       ++ [(clDist c c2, sortedPair (i, _j)) | (i, c) <- IMap.assocs cls, i /= _i, i /= _j]

        sortedPair :: Ord a => (a, a) -> (a, a)
        sortedPair (a, b) | a > b = (b, a)
        sortedPair (a, b) = (a, b)

        singleUpdate :: (Double, (Int, Int)) -> Map.Map Double (Set.Set (Int, Int)) -> Map.Map Double (Set.Set (Int, Int))
        singleUpdate (d, (i, j)) mp = 
          if Set.null ns 
            then Map.delete d mp
            else Map.insert d ns mp      
          where
            ns = -- trace (show (d, (i, j)) ++ " " ++ (show $ Map.member d mp)) $ 
                 Set.delete (i, j) $ mp Map.! d
-}                 

