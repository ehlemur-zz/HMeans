import HMeans

import Data.Ix
import Debug.Trace
import Data.Foldable
import System.Random
import Algorithms.Hungarian

import qualified Data.Vector.Unboxed as UV


----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

dimension :: Int
dimension = 76

nDigits :: Int
nDigits = 2000

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

readMFEAT :: IO [BasicData DoubleVector]
readMFEAT = do 
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

main = do 
  let mfeatParams = Params 1000 10 nDigits dimension (KMeansParams 1000)

  putStrLn $ "HMeans test script:"

  mfeatData <- readMFEAT
  randomGen <- getStdGen
  let microclusters = randomInitialize mfeatParams randomGen mfeatData
  
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
  let (hungarianPairs, hungarianScore) = runHungarian dimension cLabels mfeatLabels
  
--  putStrLn $ "Label list: "
--  putStrLn $ show $ zip mfeatLabels cLabels
  putStrLn $ "Score:"
  putStrLn $ "\t" ++ show hungarianScore ++ " points mislabeled (" ++ (show $ 100 * hungarianScore / (toSomething nDigits)) ++ "%)"
  
  putStrLn $ "---------------------------------------------------------------------------------"

--  putStrLn $ "KMeans Score:"
--  putStrLn $ "\t" ++ show hungarianScore' ++ " points mislabeled (" ++ (show $ 100 * hungarianScore' / (toSomething nDigits)) ++ "%)"

  putStrLn $ "Done" 



