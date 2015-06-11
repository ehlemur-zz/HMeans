module HMeans.Common where


import Data.Clustering.Hierarchical

import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as UV


newtype Partition  a = Partition {getPartition :: IMap.IntMap (Cluster a)} 
newtype BasicData  a = BasicData {getData :: (a, Int)}
                       deriving Show
type DoubleList   = [Double]
-- newtype DoubleVector = DoubleVector {getVector :: UV.Vector Double}
--                       deriving Show
type DoubleVector = UV.Vector Double
type DoubleIntMap = IMap.IntMap Double


data Cluster a    = Cluster {nPoints :: Int, 
                             lSum :: a, 
                             sSum :: a,
                             pointsIn :: ISet.IntSet,
                             pointsOut :: ISet.IntSet} 
                    deriving (Eq, Ord)

data HMeansParams = HierarchicalParams {linkageType :: Linkage} 
                    | KMeansParams {maxIters :: Int}
                    deriving Show

data Params       = Params {nMicroClusters :: Int,
                            nClusters :: Int,
                            nDataPoints :: Int,
                            nDims :: Int,
                            hParams :: HMeansParams} 
                    deriving Show


data DistTable = DistTable {getTable :: IMap.IntMap (Map.Map Double ISet.IntSet)}


instance (Show a) => Show (Partition a) where
    show (Partition p) = "Partition:" ++ (concat $ IMap.elems $ IMap.mapWithKey showCluster p)
      where 
        showCluster k c = "\n\tCluster #" ++ show k ++ ": " ++ show c

instance Show a => Show (Cluster a) where
    show (Cluster n l _ p _) = "Cluster with " ++ show n ++ " points" -- , namely " ++ show p

