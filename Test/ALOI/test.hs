import HMeans

import Debug.Trace
import Data.Foldable
import System.Random
import Algorithms.Hungarian

import qualified Data.Vector.Unboxed as UV

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

lineToDoubleVector :: Int -> [String] -> DoubleVector
lineToDoubleVector n s = UV.fromList $ map read $ take n s

----------------------------------------------------------------------------------------------------

lineToLabel :: Int -> [String] -> Int
lineToLabel n s = read $ drop 4 $ dropLast $ head $ drop n s
  where
    dropLast :: [a] -> [a]
    dropLast [_] = []
    dropLast (x:xs) = x : dropLast xs

----------------------------------------------------------------------------------------------------

readALOI :: IO [BasicData DoubleVector]
readALOI = do 
  let dimension = 7 * 2 * 2
  csv_input <- readFile "aloi_input.csv"

  return $ toBasicData $ map (lineToDoubleVector dimension . words) $ lines csv_input

----------------------------------------------------------------------------------------------------

readLabels :: IO [Int]
readLabels = do
  let dimension = 7 * 2 * 2
  csv_input <- readFile "aloi_input.csv"

  return $ map (lineToLabel dimension . words) $ lines csv_input

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

runHungarian :: Int -> [Int] -> [Int] -> ([(Int, Int)], Double)
runHungarian d x y = hungarian m d d
  where
      m = foldr' ($) (take (d*d) $ repeat 0) $ fmap addAllExcept $ zip x y

----------------------------------------------------------------------------------------------------

addAllExcept :: (Int, Int) -> [Double] -> [Double]
addAllExcept (i, j) = addAllExcept' (10 * i) (10 * (i + 1)) (10*i + j) 0
  where
    addAllExcept' :: Int -> Int -> Int -> Int -> [Double] -> [Double]
    addAllExcept' _ _ _ _ [] = []
    addAllExcept' fr to n i (x:xs) | fr <= i && i <= to && i /= n    = x + 1 : addAllExcept' fr to n (i+1) xs
                                   | otherwise                       = x     : addAllExcept' fr to n (i+1) xs

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

main = do 

  let nImages = 110250
  let dimension = 7 * 2 * 2

  let aloiParams = Params 3000 1000 nImages dimension (KMeansParams 1000)

  putStrLn $ "HMeans test script:"

  aloiData <- readALOI
  randomGen <- getStdGen
  let microclusters = randomInitialize aloiParams randomGen aloiData
  
  putStrLn $ "Base partition selected:"
  putStrLn $ show microclusters
  
  putStrLn $ "---------------------------------------------------------------------------------"

  aloiData' <- readALOI
  let traind = train $ map (trainDataPoint microclusters) aloiData'

  putStrLn $ "Resulting partition:"
  putStrLn $ show traind
  
  putStrLn $ "---------------------------------------------------------------------------------"
  
  let clusters = runHMeans aloiParams traind

  putStrLn $ "Clusters found:"
  putStrLn $ show clusters
  
  putStrLn $ "---------------------------------------------------------------------------------"

  aloiLabels <- readLabels
  let cLabels = partitionToLabelList clusters
  let (hungarianPairs, hungarianScore) = runHungarian dimension cLabels aloiLabels
  
  putStrLn $ "Label list: "
  putStrLn $ show $ zip aloiLabels cLabels
--  putStrLn $ "Score:"
--  putStrLn $ "\t" ++ show hungarianScore ++ " points mislabeled (" ++ (show $ 100 * hungarianScore / (toSomething nImages)) ++ "%)"
  
  putStrLn $ "---------------------------------------------------------------------------------"

--  putStrLn $ "KMeans Score:"
--  putStrLn $ "\t" ++ show hungarianScore' ++ " points mislabeled (" ++ (show $ 100 * hungarianScore' / (toSomething nImages)) ++ "%)"

  putStrLn $ "Done" 



