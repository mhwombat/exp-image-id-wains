------------------------------------------------------------------------
-- |
-- Module      :  LearningTest
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can learn to classify numerals.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Util (shuffle)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.BrainInternal (classifier, predictor,
  makeBrain)
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..),
  modelMap, Difference)
import ALife.Creatur.Wain.Image
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Numeral.Action (Action(..), correct,
  numActions, correctActions)
import ALife.Creatur.Wain.Numeral.Experiment
import ALife.Creatur.Wain.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Response (Response, action)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, doubleToPM1)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Lens
import Control.Monad (when, foldM)
import Control.Monad.Random (evalRandIO)
import Data.ByteString as BS (readFile, writeFile)
import qualified Data.Map.Strict as M
import Data.List (minimumBy, foldl')
import Data.Ord (comparing)
import qualified Data.Serialize as DS (decode, encode)
import Data.Word (Word8, Word64)
import System.Directory
import System.FilePath.Posix (takeFileName)
import System.Environment (getArgs)

reward :: Double
reward = 0.1

runAction :: Action -> Object Action -> ImageWain -> ImageWain
runAction a obj w =
  if correct a (objectNum obj)
    then wCorrect
    else wIncorrect
  -- Reward should be the same as they get from imprinting. Otherwise
  -- they may be tempted to experiment with other classifications.
  where (wCorrect, _) = adjustEnergy reward w
        (wIncorrect, _) = adjustEnergy (-reward) w

data Params = Params
  {
    nTrainingImages :: Int,
    nTestImages :: Int,
    reportNovelty :: Bool,
    trainingDir :: FilePath,
    testDir :: FilePath,
    cr0 :: UIDouble,
    crf :: UIDouble,
    ctf :: Word64,
    cdt :: UIDouble,
    cSize :: Word64,
    pr0 :: UIDouble,
    prf :: UIDouble,
    ptf :: Word64,
    pdt :: UIDouble,
    defO :: [PM1Double],
    depth :: Word8
  } deriving (Show, Read)

testWain :: Params -> ImageWain
testWain p = w'
  where wName = "Fred"
        wAppearance = bigX 28 28
        (Right wBrain) = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec (cSize p) (cdt p) ImageTweaker
        wMuser = makeMuser (defO p) (depth p)
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wRds = [0.1, 0, 0, 0]
        wPredictorSize = cSize p * fromIntegral numActions
        wPredictor = buildPredictor ep wPredictorSize (pdt p)
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        ec = LearningParams (cr0 p) (crf p) (ctf p)
        ep = LearningParams (pr0 p) (prf p) (ptf p)
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

-- trainOne :: ImageWain -> Object Action -> ImageWain
-- trainOne w obj = wainFinal
--   where (_, _, _, _, r, wainAfterDecision) = chooseAction [objectAppearance obj] w
--         a = view action r
--         wainRewarded = runAction a obj wainAfterDecision
--         (wainAfterReflection, _) = reflect [objectAppearance obj] r wainAfterDecision wainRewarded
--         -- keep the wain's energy constant
--         restorationEnergy = uiToDouble (view energy w) - uiToDouble (view energy wainRewarded)
--         -- keep the wain's boredom constant
--         restorationBoredom = uiToDouble (view boredom w) - uiToDouble (view boredom wainRewarded)
--         (wainPartiallyRestored, _) = adjustEnergy restorationEnergy wainAfterReflection
--         (wainFinal, _) = adjustBoredom restorationBoredom wainPartiallyRestored

imprintOne :: ImageWain -> Object Action -> ImageWain
imprintOne w obj = w'
  where (_, _, _, _, w') = imprint [objectAppearance obj] a w
        a = correctActions !! (objectNum obj)

testOne :: Bool -> ImageWain -> Int -> Object Action -> IO Int
testOne rn w k obj = do
  let (lds, _, _, _, r, _) = chooseAction [objectAppearance obj] w
  when rn $
    putStrLn $ "Novelty of " ++ objectId obj ++ " is "
      ++ show (novelty lds)
  let a = view action r
  if correct a (objectNum obj)
    then return $ k+1
    else return k

describeClassifierModels :: ImageWain -> IO ()
describeClassifierModels w = mapM_ (putStrLn . f) ms
  where ms = M.toList . modelMap . view (brain . classifier) $ w
        f (l, r) = show l ++ ": <img src='data:image/png;base64,"
                     ++ base64encode r ++ "'/>"

describePredictorModels :: ImageWain -> IO ()
describePredictorModels w = describePredictorModels' ms
  where ms = M.toList . modelMap . view (brain . predictor) $ w

describePredictorModels' :: [(Label, Response Action)] -> IO ()
describePredictorModels' [] = return ()
describePredictorModels' xs = do
  putStrLn $ concatMap f ys
  describePredictorModels' zs
  where (ys, zs) = splitAt 4 xs
        f (l, r) = show l ++ ": " ++ pretty r ++ " "

novelty  :: [[(Label, Difference)]] -> UIDouble
novelty (lds:_) = 1 - snd (minimumBy (comparing snd) lds)
novelty _ = error "attempted to calculate novelty on empty list"

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  files <- map (d ++) . drop 2 <$> getDirectoryContents d
  evalRandIO (shuffle files)

readImage2 :: FilePath -> IO (Object Action)
readImage2 f = do
  img <- readImage f
  return $ IObject img (takeFileName f)

trial :: [Object Action] -> [Object Action] -> Params -> IO ()
trial xs ys p = do
  w <- fetchWain p
  -- let wTrained = foldl' trainOne w xs
  let wTrained = foldl' imprintOne w xs
  k <- foldM (testOne (reportNovelty p) wTrained) 0 ys
  let fractionCorrect = fromIntegral k / fromIntegral (nTestImages p) :: Double
  putStrLn $ show fractionCorrect ++ ", " ++ show k ++ ", " ++ show p ++ ", " ++ show (stats wTrained)
    ++ " " ++ versionInfo
  writeWain wTrained

wainFile :: FilePath
wainFile = "superwain"

fetchWain :: Params -> IO ImageWain
fetchWain p = do
  exists <- doesFileExist wainFile
  if exists
    then do
      putStrLn "Using existing wain"
      x <- BS.readFile wainFile
      let (Right w) = DS.decode x
      return w
    else do
      putStrLn "Creating new wain"
      return $ testWain p

writeWain :: ImageWain -> IO ()
writeWain w = do
  let x = DS.encode w
  BS.writeFile wainFile x

main :: IO ()
main = do
  params <- read . head <$> getArgs
  trainingFiles <- take (nTrainingImages params) <$> readDirAndShuffle (trainingDir params)
  trainingImages <- mapM readImage2 trainingFiles
  testFiles <- take (nTestImages params) <$> readDirAndShuffle (testDir params)
  testImages <- mapM readImage2 testFiles
  trial trainingImages testImages params

