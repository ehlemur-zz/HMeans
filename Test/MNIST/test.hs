import Debug.Trace

import Data.Word
import System.Random
import Data.Bits
import Data.Foldable
import System.Endian
import Data.Binary.Get
import Criterion.Measurement
import Control.Exception.Base
import qualified Data.IntMap as IMap
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as UV

import Algorithms.Hungarian

import HMeans



getInt32 :: BL.ByteString -> (Int, BL.ByteString)
getInt32 b = (a', rest)
  where 
    (a, rest) = getInt32' 4 b
    a' = a --toNum $ fromBE32 $ fromInteger $ toNumeger a

    getInt32' :: Int -> BL.ByteString -> (Int, BL.ByteString)
    getInt32' 0 bs = (0, bs)
    getInt32' n bs = ((shift (toNum (BL.head bs)) (8 * (n - 1))) + m, bs')
      where
        (m, bs') = getInt32' (n - 1) (BL.tail bs)

getNum8 :: Num a => BL.ByteString -> (a, BL.ByteString)
getNum8 b = (toNum $ BL.head b, BL.tail b)



getImages :: Int -> Int -> BL.ByteString -> [DoubleIntMap]
getImages n sz b = fst $ getImages' n sz b
  where
    getImages' 0 _ b = ([], b)
    getImages' n sz b = (img : imgs, b'')
      where 
        (img, b') = getImage sz b
        (imgs, b'') = getImages' (n - 1) sz b'

    getImage :: Int -> BL.ByteString -> (DoubleIntMap, BL.ByteString)
    getImage sz b = (vs, rest)
      where
        (ds, rest) = getImage' sz b
        vs = IMap.fromList ds

        getImage' 0 b = ([], b)
        getImage' n b = if pixel > 0 
                          then ((n, pixel) : pixels, b'')
                          else (pixels, b'')
          where
            (pixel, b') = getNum8 b
            (pixels, b'') = getImage' (n-1) b'

readImages :: BL.ByteString -> ([DoubleIntMap], Int)
readImages b = (getImages n sz b', n)
  where
    (_, b2) = getInt32 b
    (n, b3) = getInt32 b2
    (w, b4) = getInt32 b3
    (h, b') = getInt32 b4
    sz = w * h


getLabels :: Int -> BL.ByteString -> [Int]
getLabels n = getLabels' n
  where
    getLabels' 0 _ = []
    getLabels' n b = i : getLabels' (n-1) b'
      where
        (i, b') = getNum8 b

readLabels :: BL.ByteString -> [Int]
readLabels b = getLabels n b'
  where
    (_, b2) = getInt32 b
    (n, b') = getInt32 b2


addAllExcept :: (Int, Int) -> [Double] -> [Double]
addAllExcept (i, j) = addAllExcept' (10 * i) (10 * (i + 1)) (10*i + j) 0
  where
    addAllExcept' :: Int -> Int -> Int -> Int -> [Double] -> [Double]
    addAllExcept' _ _ _ _ [] = []
    addAllExcept' fr to n i (x:xs) | fr <= i && i <= to && i /= n    = x + 1 : addAllExcept' fr to n (i+1) xs
                                   | otherwise                       = x     : addAllExcept' fr to n (i+1) xs


runHungarian :: [Int] -> [Int] -> ([(Int, Int)], Double)
runHungarian x y = hungarian m 10 10
  where
    m = foldr' ($) (take 100 $ repeat 0) $ fmap addAllExcept $ zip x y


main = do 
  let loopHierarchical :: HMeansParams -> Int -> IO () 
      loopHierarchical hParameters nImages = 
        do let params = Params undefined 10 nImages (28*28) hParameters
           t        <- getTime
           
           labels   <- BL.readFile "mnist_labels"
                            >>= wrap readLabels
                            >>= wrap (take nImages)
           
           clusters <- BL.readFile "mnist_input" 
                            >>= wrap (fst . readImages) 
                            >>= wrap (take nImages) 
                            >>= wrap toBasicData
                            >>= wrap (map $ toCluster)
                            >>= wrap (zip [0..])
                            >>= wrap (Partition . IMap.fromList)
                            >>= wrap (runHierarchical params)           

           evaluate clusters
                        
           t'       <- getTime

           let kmeansResult = partitionToLabelList clusters
           let (hungarianPairs', hungarianScore') = runHungarian kmeansResult labels

           putStrLn $ "Hierachical Score with " ++ show nImages ++ " points:"
           putStrLn $ "  " ++ show hungarianScore' ++ " points mislabeled (" ++ (show $ 100 * hungarianScore' / (toNum nImages)) ++ "%)"
           putStrLn $ "  " ++ (secs $ t' - t)
     
           if nImages + 200 > 60000 then
             return ()
           else
             loopHierarchical hParameters (nImages + 200)

  let loopHMeans :: HMeansParams -> Int -> Int -> IO () 
      loopHMeans hParameters nMicro nImages = 
        do let params = Params nMicro 10 nImages (28*28) hParameters
           t             <- getTime
           
           labels        <- BL.readFile "mnist_labels"
                            >>= wrap readLabels
                            >>= wrap (take nImages)
           
           randomSeed    <- getStdGen

           microclusters <- BL.readFile "mnist_input" 
                            >>= wrap (fst . readImages) 
                            >>= wrap (take nImages) 
                            >>= wrap toBasicData
                            >>= wrap (randomInitialize params randomSeed)
           
           clusters      <- BL.readFile "mnist_input"
                            >>= wrap (fst . readImages) 
                            >>= wrap (take nImages) 
                            >>= wrap toBasicData
                            >>= wrap (map $ trainSingle microclusters)
                            >>= wrap train
                            >>= wrap (runHMeans params)

           evaluate clusters
                        
           t'            <- getTime

           let kmeansResult = partitionToLabelList clusters
           let (hungarianPairs', hungarianScore') = runHungarian kmeansResult labels

           putStrLn $ "HMeans Score with " ++ show nImages ++ " points:"
           putStrLn $ "  " ++ show hungarianScore' ++ " points mislabeled (" ++ (show $ 100 * hungarianScore' / (toNum nImages)) ++ "%)"
           putStrLn $ "  " ++ (secs $ t' - t)
     
           if nImages + 200 > 60000 then
             return ()
           else
             loopHMeans hParameters nMicro (nImages + 200)

  let loopKMeans :: HMeansParams -> Int -> IO () 
      loopKMeans hParameters nImages = 
        do let params = Params undefined 10 nImages (28*28) hParameters
           t        <- getTime

           labels   <- BL.readFile "mnist_labels"
                            >>= wrap readLabels
                            >>= wrap (take nImages)

           clusters <- runKMeansIO params "mnist_input" 
                       (\x -> (BL.readFile x >>= wrap (fst . readImages) >>= wrap (take nImages) >>= wrap toBasicData))
              
           t'       <- getTime

           let kmeansResult = partitionToLabelList clusters
           let (hungarianPairs', hungarianScore') = runHungarian kmeansResult labels
           
           putStrLn $ "KMeans Score with " ++ show nImages ++ " points:"
           putStrLn $ "  " ++ show hungarianScore' ++ " points mislabeled (" ++ (show $ 100 * hungarianScore' / (toNum nImages)) ++ "%)"
           putStrLn $ "  " ++ (secs $ t' - t)
     
           if nImages + 200 > 60000 then
             return ()
           else
             loopKMeans hParameters (nImages + 200)

  initializeTime

  loopHMeans (HierarchicalParams UPGMA) 100 20
--  loopHierarchical (HierarchicalParams UPGMA) 20

--  loopHMeans (KMeansParams 1000) 100 20
--  loopKMeans (KMeansParams 1000) 20 

