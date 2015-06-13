import HMeans

import Data.Ix
import Debug.Trace
import Control.Monad
import Data.Foldable
import System.Random
import Algorithms.Hungarian
import Criterion.Measurement

import qualified Data.Vector.Unboxed as UV



----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

lineToDoubleVector :: Int -> [String] -> DoubleVector
lineToDoubleVector n s = UV.fromList $ map read s

----------------------------------------------------------------------------------------------------

lineToLabel :: Int -> [String] -> Int
lineToLabel n s = read $ drop 4 $ dropLast $ head $ drop n s
  where
    dropLast :: [a] -> [a]
    dropLast [_] = []
    dropLast (x:xs) = x : dropLast xs

----------------------------------------------------------------------------------------------------

readMFEAT :: Int -> IO [BasicData DoubleVector]
readMFEAT dimension = do 
  txt_input <- readFile "mfeat_input.txt"

  return $ toBasicData $ map (lineToDoubleVector dimension . words) $ lines txt_input

----------------------------------------------------------------------------------------------------

readLabels :: IO [Int]
readLabels = return [i | i <- range(0, 9) , _ <- range(1, 200)]

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

runKWith :: Int -> Int -> Int -> [BasicData DoubleVector] -> [Int] -> IO Double
runKWith nClasses nDigits dimension mfeatData mfeatLabels = do
  let mfeatParams = Params nDigits nClasses nDigits dimension (KMeansParams 1000)

  randomGen <- getStdGen
  let microclusters = initialize mfeatParams mfeatData
  let clusters = runHMeans mfeatParams microclusters

  let cLabels = partitionToLabelList clusters
  let (hungarianPairs, hungarianScore) = runHungarian nClasses cLabels mfeatLabels
  
  return $ 100 * hungarianScore / (toSomething nDigits)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

runWith :: Int -> Int -> Int -> Int -> [BasicData DoubleVector] -> [Int] -> IO Double
runWith nMicroclusters nClasses nDigits dimension mfeatData mfeatLabels = do
  let mfeatParams = Params nMicroclusters nClasses nDigits dimension (KMeansParams 1000)

  randomGen <- getStdGen
  let microclusters = randomInitialize mfeatParams randomGen mfeatData
  
  let traind = train $ map (trainDataPoint microclusters) mfeatData
  let clusters = runHMeans mfeatParams traind

  let cLabels = partitionToLabelList clusters
  let (hungarianPairs, hungarianScore) = runHungarian nClasses cLabels mfeatLabels
  
  return $ 100 * hungarianScore / (toSomething nDigits)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

loop :: Int -> [BasicData DoubleVector] -> [Int] -> IO ()
loop n mfeatData mfeatLabels = do
  
  d <- runWith n 10 2000 76 mfeatData mfeatLabels
  t <- getCPUTime
  putStrLn $ show n ++ " " ++ show d ++ " " ++ show t
  if n + 200 == 2011
    then loop 2000 mfeatData mfeatLabels
  else if n + 100 > 2000
    then return()
  else loop (n + 200) mfeatData mfeatLabels
  
main = do
  mfeatData <- readMFEAT 76
  mfeatLabels <- readLabels

  initializeTime

  d <- runKWith 10 2000 76 mfeatData mfeatLabels
  t <- getCPUTime
  putStrLn $ show d ++ " " ++ show t
  
  loop 11 mfeatData mfeatLabels

  d <- runKWith 10 2000 76 mfeatData mfeatLabels
  t <- getCPUTime
  putStrLn $ show d ++ " " ++ show t



{-  let mfeatParams = Params nMicroclusters nClasses nDigits dimension (KMeansParams 1000)

  putStrLn $ "HMeans test script:"

  mfeatData <- readMFEAT
  randomGen <- getStdGen
  let microclusters = randomInitialize mfeatParams randomGen mfeatData
--  let microclusters = initialize mfeatParams mfeatData
  
  putStrLn $ "Base partition selected:"
  putStrLn $ show microclusters
  
  putStrLn $ "---------------------------------------------------------------------------------"

  mfeatData' <- readMFEAT
  let traind = train $ map (trainDataPoint microclusters) mfeatData'

  putStrLn $ "Resulting partition:"
  putStrLn $ show traind
  
  putStrLn $ "---------------------------------------------------------------------------------"
  
  let clusters = runHMeans mfeatParams traind

  putStrLn $ "Clusters found:"
  putStrLn $ show clusters
  
  putStrLn $ "---------------------------------------------------------------------------------"

  mfeatLabels <- readLabels
  let cLabels = partitionToLabelList clusters
  let (hungarianPairs, hungarianScore) = runHungarian nClasses cLabels mfeatLabels
  
  putStrLn $ "Label list: "
  putStrLn $ show $ zip mfeatLabels cLabels
  
  putStrLn $ "---------------------------------------------------------------------------------"

  putStrLn $ "Score:"
  putStrLn $ "\t" ++ show hungarianScore ++ " points mislabeled (" ++ (show $ 100 * hungarianScore / (toSomething nDigits)) ++ "%)"
  
  putStrLn $ "---------------------------------------------------------------------------------"

--  putStrLn $ "KMeans Score:"
--  putStrLn $ "\t" ++ show hungarianScore' ++ " points mislabeled (" ++ (show $ 100 * hungarianScore' / (toSomething nDigits)) ++ "%)"

  putStrLn $ "Done" 


-}
