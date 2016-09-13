------------------------------------------------------------------------
-- |
-- Module      :  LearningTest
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can learn.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal (classifier, predictor,
  makeBrain, scenarioReport, responseReport, decisionReport,
  decisionQuality)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..))
import ALife.Creatur.Wain.Image.Pattern
import qualified ALife.Creatur.Wain.Image.Wain as IW
import ALife.Creatur.Wain.Image.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.ImageID.Action (Action(..), correct)
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
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Directory
import System.FilePath.Posix (takeFileName)

numTests :: Int
numTests = 1000

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
        wClassifier = buildClassifier ec wCSize 0.14 PatternTweaker
        wCSize = 500
        Right wMuser = makeMuser [0, 0, 0, 0] 1
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wRds = [0.1, 0, 0, 0]
        wPredictor = buildPredictor ep (wCSize*11) 0.1 ResponseTweaker
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        ec = LearningParams 1 0.001 (fromIntegral numTests)
        ep = LearningParams 1 0.001 (fromIntegral numTests)
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

putHtml :: String -> IO ()
putHtml s = putStr s 

putHtmlLn :: String -> IO ()
putHtmlLn s = putStrLn $ s ++ "<br/>"

tryOne
  :: PatternWain -> Object Action (ResponseTweaker Action)
    -> IO (PatternWain)
tryOne w obj = do
  putHtmlLn $ "-----<br/>"
  putHtmlLn $ "stats=" ++ show (stats w)
  let (ldss, sps, rplos, aohs, r, wainAfterDecision)
        = chooseAction [objectAppearance obj] w
  putStrLn $ "ldss=" ++ show ldss
  let (cBMU, _) = minimumBy (comparing snd) . head $ ldss
  mapM_ putHtmlLn $ scenarioReport sps
  mapM_ putHtmlLn $ responseReport rplos
  mapM_ putHtmlLn $ decisionReport aohs
  putHtmlLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ wainAfterDecision)
  mapM_ putHtmlLn $ IW.describeClassifierModels wainAfterDecision
  mapM_ putHtmlLn $ IW.describePredictorModels wainAfterDecision
  let a = view action r
  putHtmlLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " and chooses to " ++ show a
    ++ " predicting the outcomes " ++ show (view outcomes r)
  putHtmlLn $ "<img src='data:image/png;base64,"
                     ++ base64encode (objectAppearance obj) ++ "'/>"
  let wainRewarded = runAction a obj wainAfterDecision
  let deltaH = uiToDouble (happiness wainRewarded) - uiToDouble (happiness w)
  putHtmlLn $ "Δh=" ++ show deltaH
  putHtmlLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putHtmlLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  putHtml $ "Choosing to " ++ show a ++ " in response to " ++ objectId obj
  if correct a (objectNum obj)
    then putHtmlLn " was correct"
    else putHtmlLn " was wrong"
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

dir :: String
dir = "/home/eamybut/mnist/trainingData/"

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  files <- map (d ++) . drop 2 <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readOneSample :: FilePath -> IO (Object Action (ResponseTweaker Action))
readOneSample f = do
  img <- readImage f
  return $ PObject img (takeFileName f)

main :: IO ()
main = do
  putHtmlLn $ "numTests=" ++ show numTests
  files <- take numTests . drop 2 <$> readDirAndShuffle dir
  imgs <- mapM readOneSample files
  _ <- foldM tryOne testWain imgs
  putHtmlLn "test complete"

