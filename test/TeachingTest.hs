------------------------------------------------------------------------
-- |
-- Module      :  TeachingTest
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can be taught.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur (agentId)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal (classifier, predictor,
  makeBrain, decisionQuality)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..))
import ALife.Creatur.Wain.Image.Pattern
import qualified ALife.Creatur.Wain.Image.Wain as IW
import ALife.Creatur.Wain.Image.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.ImageID.Action (Action(..), correct,
  correctActions)
import ALife.Creatur.Wain.ImageID.Experiment
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.PlusMinusOne (doubleToPM1)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Response (action, outcomes)
import ALife.Creatur.Wain.SimpleResponseTweaker (ResponseTweaker(..))
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import ALife.Creatur.Util (shuffle)
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.Random (evalRand, mkStdGen)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Directory
import System.FilePath.Posix (takeFileName)

reward :: Double
reward = 0.1

runAction
  :: Action -> Object Action (ResponseTweaker Action) -> PatternWain
    -> PatternWain
runAction a obj w =
  if correct a (objectNum obj)
    then wCorrect
    else wIncorrect
  where (wCorrect, _) = adjustEnergy reward w
        (wIncorrect, _) = adjustEnergy (-reward) w
        
testWain :: PatternWain
testWain = w'
  where wName = "Fred"
        wAppearance = bigX 28 28
        Right wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 32 wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec wCSize 0.12 PatternTweaker
        wCSize = 1000
        Right wMuser = makeMuser [-1, -1, -1, -1] 1
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wRds = [0.1, 0, 0, 0]
        wPredictor = buildPredictor ep (wCSize*11) 0.1 ResponseTweaker
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        -- The classifier does most of its learning by round 100.
        ec = LearningParams 0.1 0.001 (fromIntegral numImprints)
        -- The predictor needs to keep learning longer.
        ep = LearningParams 0.1 0.0001 (fromIntegral numImprints)
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

putHtml :: String -> IO ()
putHtml s = putStr s 

putHtmlLn :: String -> IO ()
putHtmlLn s = putStrLn $ s ++ "<br/>"

imprintOne
  :: PatternWain -> Object Action (ResponseTweaker Action)
    -> IO (PatternWain)
imprintOne w obj = do
  let a = correctActions !! (objectNum obj)
  putHtmlLn $ "Teaching " ++ agentId w ++ " that correct action for "
    ++ objectId obj ++ " is " ++ show a
  let (_, _, _, _, w') = imprint [objectAppearance obj] a w
  return w'

tryOne
  :: PatternWain -> Object Action (ResponseTweaker Action)
    -> IO (PatternWain)
tryOne w obj = do
  putHtmlLn $ "-----<br/>"
  putHtmlLn $ "stats=" ++ show (stats w)
  putHtmlLn "Initial classifier models:"
  mapM_ putHtml $ IW.describeClassifierModels w
  putHtmlLn ""
  putHtmlLn "Initial prediction models"
  mapM_ putHtmlLn $ IW.describePredictorModels w
  let (lds, _, _, _, r, wainAfterDecision) = chooseAction [objectAppearance obj] w
  let (cBMU, _):(cBMU2, _):_ = sortBy (comparing snd) . head $ lds
  let a = view action r
  putHtmlLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " (alt. " ++ show cBMU2
    ++ ") and chooses to " ++ show a
    ++ " predicting the outcomes " ++ show (view outcomes r)
  putHtmlLn $ "<img src='data:image/png;base64,"
                     ++ base64encode (objectAppearance obj) ++ "'/>"
  let wainRewarded = runAction a obj wainAfterDecision
  let deltaH = uiToDouble (happiness wainRewarded) - uiToDouble (happiness w)
  putHtmlLn $ "Δh=" ++ show deltaH
  putHtmlLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putHtmlLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  putHtml $ "Choosing to " ++ show a ++ " in response to " ++ objectId obj
  if a /= correctActions !! (objectNum obj)
    then putHtmlLn " was wrong"
    else putHtmlLn " was correct"
  let (wainAfterReflection, err) = reflect [objectAppearance obj] r w wainRewarded
  putHtmlLn $ "err=" ++ show err
  -- keep the wain's energy constant
  let restorationEnergy = uiToDouble (view energy w) - uiToDouble (view energy wainRewarded)
  -- keep the wain's boredom constant
  let restorationBoredom = uiToDouble (view boredom w) - uiToDouble (view boredom wainRewarded)
  let (wainPartiallyRestored, _) = adjustEnergy restorationEnergy wainAfterReflection
  let (wainFinal, _) = adjustBoredom restorationBoredom wainPartiallyRestored
  putHtmlLn $ "classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ w)
  putHtmlLn $ "predictor SQ=" ++ show (schemaQuality . view (brain . predictor) $ w)
  putHtmlLn $ "DQ=" ++ show (decisionQuality . view brain $ w)
  return wainFinal

trainingDir :: String
trainingDir = "/home/eamybut/mnist/trainingData/"

testDir :: String
testDir = "/home/eamybut/mnist/testData/"

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  files <- map (d ++) . drop 2 <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readOneSample :: FilePath -> IO (Object Action (ResponseTweaker Action))
readOneSample f = do
  img <- readImage f
  return $ PObject img (takeFileName f)

numImprints :: Int
numImprints = 1000

numTests :: Int
numTests = 1000

main :: IO ()
main = do
  putHtmlLn $ "numImprints=" ++ show numImprints
  putHtmlLn $ "numTests=" ++ show numTests
  putHtmlLn $ "stats=" ++ show (stats testWain)
  imprintFiles <- take (numImprints + numTests) . drop 2 <$> readDirAndShuffle trainingDir
  imprintSamples <- mapM readOneSample imprintFiles
  testFiles <- take numTests . drop 2 <$> readDirAndShuffle testDir
  testSamples <- mapM readOneSample testFiles
  imprintedWain <- foldM imprintOne testWain imprintSamples
  putHtmlLn "Imprinted classifier models:"
  mapM_ putHtml $ IW.describeClassifierModels imprintedWain
  putHtmlLn ""
  putHtmlLn "Imprinted prediction models"
  mapM_ putHtmlLn $ IW.describePredictorModels imprintedWain
  _ <- foldM tryOne imprintedWain testSamples
  putHtmlLn "test complete"

