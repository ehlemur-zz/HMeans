
import HMeans

import Control.Monad
import System.Random
import Data.Binary.Get

import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IMap



parseLabels :: Get [Int]
parseLabels =
  do getWord32be -- magic number
     nLabels <- liftM toNum getWord32be
     replicateM nLabels $ liftM toNum getWord8


parseImages :: Get [DoubleIntMap]
parseImages = 
  do getWord32be -- magic number
     nImages <- liftM toNum getWord32be
     w <- liftM toNum getWord32be
     h <- liftM toNum getWord32be

     let imageSize = w * h

     let loopImage :: Int -> Get [(Int, Double)]
         loopImage 0  = return []
         loopImage sz = 
           do d <- liftM toNum getWord8
              ds <- loopImage (sz - 1)
              if d /= 0 then 
                return $ (sz, d) : ds
              else 
                return ds  

     replicateM nImages (loopImage imageSize >>= (return . IMap.fromList))

       

labelSource :: String -> IO [Int]
labelSource inputFile = 
  do file <- BL.readFile inputFile
     return []

inputSource :: String -> IO [BasicData DoubleIntMap]
inputSource inputFile = 
  do fileStream <- BL.readFile inputFile 
     return $ toBasicData $ runGet parseImages fileStream 


main = 
  do let inputFilename = "mnist_input"
     let labelFilename = "mnist_labels"

     let nImages = 60000
     let nMicroclusters = 100
     let nDimensions = 28 * 28
     let nClusters = 10
     let maxIters = 1000
     let hmeansParameters = KMeansParams maxIters

     let parameters = Params nMicroclusters nClusters nImages nDimensions hmeansParameters

     randomSeed <- getStdGen
     --microclusters <- inputSource inputFilename >>= wrap (randomInitialize parameters randomSeed)
     --clusters <- inputSource inputFilename >>= wrap (map $ trainSingle microclusters) >>= wrap train >>= wrap (runHMeans parameters)
     
     kmeansClusters <- runKMeansIO parameters inputFilename inputSource

     putStrLn $ show $ kmeansClusters
      
     return ()
