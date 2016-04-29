------------------------------------------------------------------------
-- |
-- Module      :  WainSGMComparison
-- Copyright   :  (c) Amy de Buitléir 2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Compare wain with similarly-configured SOM.
-- No learning occurs after the imprint (training) phase.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur (agentId)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal (makeBrain, classifier)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..),
  modelMap, currentLearningRate, patternMap)
import ALife.Creatur.Wain.Image
import qualified ALife.Creatur.Wain.ImageWain as IW
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Numeral.Action (Action(..), correct,
  correctActions, numeralFor)
import ALife.Creatur.Wain.Numeral.Experiment
import ALife.Creatur.Wain.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.PlusMinusOne (doubleToPM1)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Response (action)
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI)
import ALife.Creatur.Wain.Weights (makeWeights)
import ALife.Creatur.Util (shuffle)
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.Random (evalRand, mkStdGen)
import qualified Data.Datamining.Clustering.SGM as S
import Data.Function (on)
import Data.List (sortBy, groupBy, maximumBy)
import Data.List.Split (chunksOf)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map, insertWith, elems, empty, size)
import Data.Ord (comparing)
import Data.Word (Word64)
import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix (takeFileName)

-- import Data.ByteString as BS (readFile, writeFile)
-- import qualified Data.Serialize as DS 
--   (Serialize, decode, encode)

type Numeral = Char

reward :: Double
reward = 0.1

runAction :: Action -> Object Action -> ImageWain -> ImageWain
runAction a obj w =
  if correct a (objectNum obj)
    then wCorrect
    else wIncorrect
  where (wCorrect, _) = adjustEnergy reward w
        (wIncorrect, _) = adjustEnergy (-reward) w

type ImageSGM = S.SGM Word64 UIDouble Label Image

putImageGrid :: Int -> [Image] -> IO ()
putImageGrid n xs = mapM_ putImageRow (chunksOf n xs)

putImageRow :: [Image] -> IO ()
putImageRow xs = mapM_ (putStr . imageToHtml) xs >> putStrLn ""

imageToHtml :: Image -> String
imageToHtml x = "<img src='data:image/png;base64," ++ base64encode x ++ "'/>"

testSGM :: UIDouble -> UIDouble -> UIDouble -> ImageSGM
testSGM threshold r0 rf
  = S.makeSGM sgmLearningFunction wCSize threshold False imageDiff makeImageSimilar
  where wCSize = 2000
        sgmLearningFunction t = r0 * ((rf/r0)**a)
          where a = doubleToUI $ fromIntegral t / 60000

testWain :: UIDouble -> UIDouble -> UIDouble -> UIDouble -> UIDouble -> ImageWain
testWain threshold r0c rfc r0p rfp = w'
  where wName = "Fred"
        wAppearance = bigX 28 28
        Right wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 32 wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec wCSize threshold ImageTweaker
        wCSize = 2000
        wMuser = makeMuser [-0.01, -0.01, -0.01, -0.01] 1
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wRds = [0.1, 0, 0, 0]
        wPredictor = buildPredictor ep (wCSize*11) 0.1
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        ec = LearningParams r0c rfc 60000
        ep = LearningParams r0p rfp 60000
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

type ModelCreationData = Map Label (Numeral,Numeral)

updateModelCreationData
  :: Label -> Numeral -> ModelCreationData -> ModelCreationData
updateModelCreationData bmu numeral modelCreationData =
  insertWith f bmu (numeral, numeral) modelCreationData
  where f (x, _) (_, y) = (x, y)

trainOne :: (ImageWain, ModelCreationData, ImageSGM) -> Object Action -> IO (ImageWain, ModelCreationData, ImageSGM)
trainOne (w, modelCreationData, sgm) obj = do
  putStrLn $ "-----"
  putStrLn $ "Wain time: " ++ show (S.time . view (brain . classifier . patternMap) $ w)
  putStrLn $ "SGM time: " ++ show (S.time sgm)
  let wainRate = currentLearningRate . view (brain . classifier) $ w
  putStrLn $ "Wain learning rate: " ++ show wainRate
  let sgmRate = (S.learningRate sgm) (S.time sgm)
  putStrLn $ "SGM learning rate: " ++ show sgmRate
  let numeral = head . show $ objectNum obj
  let a = correctActions !! objectNum obj
  putStrLn $ "Teaching " ++ agentId w ++ " that correct action for "
    ++ objectId obj ++ " is " ++ show a
  let (lds, sps, _, _, w') = imprint [objectAppearance obj] a w
  -- let bmu = head . fst $ maximumBy (comparing snd) sps
  let bmu = head . fst . maximumBy (comparing snd) . reverse $ sps
  let (sgmBMU, _, sgmLDS, sgm')  = S.trainAndClassify sgm (objectAppearance obj)
  putStrLn $ "Wain: " ++ objectId obj ++ "," ++ numeral : "," ++ show bmu
  let wainModel = (modelMap . view (brain . classifier) $ w') M.! bmu
  putStrLn . imageToHtml $ wainModel
  -- putImageGrid 10 (M.elems . modelMap . view (brain . classifier) $ w')
  putStrLn $ "SGM:  " ++ objectId obj ++ "," ++ numeral : "," ++ show sgmBMU
  let sgmModel = (S.modelMap sgm') M.! sgmBMU
  putStrLn . imageToHtml $ sgmModel
  -- putImageGrid 10 (M.elems . S.modelMap $ sgm')
  if wainRate == sgmRate
    then return ()
    else do
        putStrLn $ "Wain diffs" ++ show lds
        putStrLn $ " SGM diffs" ++ show sgmLDS
        putStrLn "Wain models:"
        putImageGrid 10 (M.elems . modelMap . view (brain . classifier) $ w')
        putStrLn "SGM models:"
        putImageGrid 10 (M.elems . S.modelMap $ sgm')
        -- BS.writeFile "amy.dat" $ DS.encode (w, w', sgm, sgm')
        error "different learning rates"
  if head lds == sgmLDS
    then return ()
    else do
        putStrLn $ "Wain diffs" ++ show lds
        putStrLn $ " SGM diffs" ++ show sgmLDS
        putStrLn "Wain models:"
        putImageGrid 10 (M.elems . modelMap . view (brain . classifier) $ w')
        putStrLn "SGM models:"
        putImageGrid 10 (M.elems . S.modelMap $ sgm')
        -- BS.writeFile "amy.dat" $ DS.encode (w, w', sgm, sgm')
        error "different diffs"
  if wainModel == sgmModel
    then return ()
    else do
        putStrLn $ "Wain diffs" ++ show lds
        putStrLn $ " SGM diffs" ++ show sgmLDS
        putStrLn "Wain models:"
        putImageGrid 10 (M.elems . modelMap . view (brain . classifier) $ w')
        putStrLn "SGM models:"
        putImageGrid 10 (M.elems . S.modelMap $ sgm')
        -- BS.writeFile "amy.dat" $ DS.encode (w, w', sgm, sgm')
        error "different models"
  if sgmBMU == bmu
    then return ()
    else do
        putStrLn $ "Wain diffs" ++ show lds
        putStrLn $ " SGM diffs" ++ show sgmLDS
        putStrLn "Wain models:"
        putImageGrid 10 (M.elems . modelMap . view (brain . classifier) $ w')
        putStrLn "SGM models:"
        putImageGrid 10 (M.elems . S.modelMap $ sgm')
        -- BS.writeFile "amy.dat" $ DS.encode (w, w', sgm, sgm')
        error "different BMUs"
  let modelCreationData' = updateModelCreationData bmu numeral modelCreationData
  return (w', modelCreationData', sgm')

testOne :: ImageWain -> [(Numeral, Bool)] -> Object Action -> IO [(Numeral, Bool)]
testOne w testStats obj = do
  putStrLn $ "-----"
  let (lds, _, _, _, r, _) = chooseAction [objectAppearance obj] w
  let (cBMU, _):(cBMU2, _):_ = sortBy (comparing snd) . head $ lds
  let a = view action r
  putStrLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " (alt. " ++ show cBMU2
    ++ ") and chooses to " ++ show a
  let numeral = head . show $ objectNum obj
  let answer = numeralFor a
  let wasCorrect = answer == numeral
  let novelty = minimum . map snd . head $ lds :: UIDouble
  putStrLn $ objectId obj ++ "," ++ numeral : "," ++ show answer
    ++ "," ++ show wasCorrect ++ "," ++ show novelty
  return $ (numeral, wasCorrect):testStats

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  let d2 = d ++ "/"
  files <- map (d2 ++) . drop 2 <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readSamples :: FilePath -> IO [Object Action]
readSamples dir = do
  files <- readDirAndShuffle dir
  mapM readOneSample files

readOneSample :: FilePath -> IO (Object Action)
readOneSample f = do
  img <- readImage f
  return $ IObject img (takeFileName f)

numeralStats :: [(Numeral, Bool)] -> [(String, Int, Int, Double)]
numeralStats xs = ("all",total,totalCorrect,fraction):xs'
  where xs' = map summarise . groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ xs
        total = sum . map (\(_, x, _, _) -> x) $ xs'
        totalCorrect = sum . map (\(_, _, x, _) -> x) $ xs'
        fraction = fromIntegral totalCorrect / fromIntegral total

summarise :: [(Numeral, Bool)] -> (String, Int, Int, Double)
summarise xs = ([n], total, numCorrect, fromIntegral numCorrect / fromIntegral total)
  where numCorrect = length $ filter snd xs
        total = length xs
        n = fst . head $ xs

countModelChanges :: ModelCreationData -> (Int, Double)
countModelChanges modelCreationData = (numChanges, fraction)
  where numChanges = length . filter f . elems $ modelCreationData
        f (a, b) = a /= b
        fraction = fromIntegral numChanges /
                     fromIntegral (size modelCreationData)

main :: IO ()
main = do
  args <- getArgs
  let trainingDir = head args
  let testDir = args !! 1
  let threshold = read $ args !! 2
  let r0c = read $ args !! 3
  let rfc = read $ args !! 4
  let r0p = read $ args !! 5
  let rfp = read $ args !! 6
  let passes  = read $ args !! 7
  putStrLn $ "trainingDir=" ++ trainingDir
  putStrLn $ "testDir=" ++ testDir
  putStrLn $ "passes=" ++ show passes
  putStrLn "====="
  putStrLn "Training"
  putStrLn "====="
  trainingSamples <- concat . replicate passes <$> readSamples trainingDir
  putStrLn "filename,numeral,label"
  let w = testWain threshold r0c rfc r0p rfp
  let s = view (brain . classifier . patternMap) w
  (trainedWain, modelCreationData, _) <- foldM trainOne (w, empty, s) trainingSamples
  putStrLn $ "stats=" ++ show (stats trainedWain)
  putStrLn ""
  putStrLn "====="
  putStrLn "Classifier models after training"
  putStrLn "====="
  mapM_ putStr $ IW.describeClassifierModels trainedWain
  putStrLn ""
  putStrLn "====="
  putStrLn "Prediction models after training"
  putStrLn "====="
  mapM_ putStrLn $ IW.describePredictorModels trainedWain
  putStrLn ""
  putStrLn "====="
  putStrLn "Testing"
  putStrLn "====="
  testSamples <- readSamples testDir
  stats2 <- foldM (testOne trainedWain) [] testSamples
  putStrLn ""
  putStrLn "====="
  putStrLn "Summary"
  putStrLn "====="
  putStrLn $ "stats=" ++ show (stats trainedWain)
  let (numModelsChanged, fractionModelsChanged) = countModelChanges modelCreationData
  putStrLn $ "number of models changed: " ++ show numModelsChanged
  putStrLn $ "fraction models changed: " ++ show fractionModelsChanged
  putStrLn "numeral,count,correct,accuracy"
  let stats3 = numeralStats stats2
  mapM_ (putStrLn . show) stats3
  putStrLn "test complete"

