------------------------------------------------------------------------
-- |
-- Module      :  RonanTest
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Generate data for publication with Ronan.
-- Imprint some images, then test on the rest.
-- No learning occurs after the imprint (training) phase.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur (agentId)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal (makeBrain)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.GeneticSOMInternal (ExponentialParams(..))
import ALife.Creatur.Wain.Image
import qualified ALife.Creatur.Wain.ImageWain as IW
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Numeral.Action (Action(..), correct,
  correctActions)
import ALife.Creatur.Wain.Numeral.Experiment
import ALife.Creatur.Wain.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.PlusMinusOne (doubleToPM1)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Response (action, outcomes)
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

runAction :: Action -> Object Action -> ImageWain -> ImageWain
runAction a obj w =
  if correct a (objectNum obj)
    then wCorrect
    else wIncorrect
  where (wCorrect, _) = adjustEnergy reward w
        (wIncorrect, _) = adjustEnergy (-reward) w
        
testWain :: ImageWain
testWain = w'
  where wName = "Fred"
        wAppearance = bigX 28 28
        Right wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 wIos
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec wCSize 0.03 ImageTweaker
        wCSize = 2000
        wMuser = makeMuser [-0.01, -0.01, -0.01, -0.01] 1
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wPredictor = buildPredictor ep (wCSize*11) 0.1
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        -- The classifier does most of its learning by round 100.
        ec = ExponentialParams 0.1 0.00015
        -- The predictor needs to keep learning longer.
        ep = ExponentialParams 0.1 0.00015
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

putHtml :: String -> IO ()
putHtml s = putStr s 

putHtmlLn :: String -> IO ()
putHtmlLn s = putStrLn $ s ++ "<br/>"

imprintOne :: ImageWain -> Object Action -> IO (ImageWain)
imprintOne w obj = do
  let a = correctActions !! (objectNum obj)
  putHtmlLn $ "Teaching " ++ agentId w ++ " that correct action for "
    ++ objectId obj ++ " is " ++ show a
  return $ imprint [objectAppearance obj] a w

tryOne :: ImageWain -> Int -> Object Action -> IO Int
tryOne w k obj = do
  putHtmlLn $ "-----<br/>"
  putHtmlLn $ "stats=" ++ show (stats w)
  putHtmlLn "Initial classifier models:"
  mapM_ putHtml $ IW.describeClassifierModels w
  putHtmlLn ""
  putHtmlLn "Initial prediction models"
  mapM_ putHtmlLn $ IW.describePredictorModels w
  let (lds, _, _, _, r, wainAfterDecision) = chooseAction [objectAppearance obj] w
  -- putHtmlLn $ "DEBUG lds=" ++ show lds
  let (cBMU, _):(cBMU2, _):_ = sortBy (comparing snd) . head $ lds
  -- putHtmlLn $ "DEBUG cBMU=" ++ show cBMU
  -- putHtmlLn $ "DEBUG sps=" ++ show sps
  -- putHtmlLn $ "DEBUG rplos=" ++ show rplos
  -- mapM_ putHtmlLn $ scenarioReport sps
  -- mapM_ putHtmlLn $ responseReport rplos
  -- mapM_ putHtmlLn $ decisionReport aohs
  -- putHtmlLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ wainAfterDecision)
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
  if correct a (objectNum obj)
    then do
      putHtmlLn " was correct"
      return $ k+1
    else do
      putHtmlLn " was wrong"
      return k

dir :: String
dir = "/home/eamybut/mnist/trainingData/"

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  files <- map (d ++) . drop 2 <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readImage2 :: FilePath -> IO (Object Action)
readImage2 f = do
  img <- readImage f
  return $ IObject img (takeFileName f)

numImprints :: Int
numImprints = 1000

numTests :: Int
numTests = 1000

main :: IO ()
main = do
  putHtmlLn $ "numImprints=" ++ show numImprints
  putHtmlLn $ "numTests=" ++ show numTests
  putHtmlLn $ "stats=" ++ show (stats testWain)
  files <- take (numImprints + numTests) . drop 2 <$> readDirAndShuffle dir
  imgs <- mapM readImage2 files
  let (imprintImages, testImages) = splitAt numImprints imgs
  imprintedWain <- foldM imprintOne testWain imprintImages
  putHtmlLn "Imprinted classifier models:"
  mapM_ putHtml $ IW.describeClassifierModels imprintedWain
  putHtmlLn ""
  putHtmlLn "Imprinted prediction models"
  mapM_ putHtmlLn $ IW.describePredictorModels imprintedWain
  k <- foldM (tryOne imprintedWain) 1 testImages
  let fractionCorrect = fromIntegral k / fromIntegral (length testImages) :: Double
  putHtmlLn $ show fractionCorrect ++ ", " ++ show k
    ++ ", " ++ show (stats imprintedWain) ++ " " ++ versionInfo
  putHtmlLn "test complete"

