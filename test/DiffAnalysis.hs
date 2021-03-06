------------------------------------------------------------------------
-- |
-- Module      :  LearningTest
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Analyse the diffs between images of the same numeral, and images of
-- different numerals.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Util (shuffle)
import ALife.Creatur.Wain.Image.Pattern
import ALife.Creatur.Wain.Statistics (mean, popStdDev)
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import Control.Monad (foldM)
import Control.Monad.Random (evalRand, mkStdGen)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath.Posix (takeFileName)

numTests :: Int
numTests = 500

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  let d2 = d ++ "/"
  files <- map (d2 ++) . drop 2 <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readImage2 :: FilePath -> IO (Pattern, Int)
readImage2 f = do
  img <- readImage f
  return (img, read . take 1 . takeFileName $ f)

run :: ([Double], [Double]) -> ((Pattern, Int), (Pattern, Int))
        -> IO ([Double], [Double])
run (intraDiffs, interDiffs) ((i1, n1), (i2, n2)) = do
  putStr $ show n1 ++ " " ++ show n2 ++ " "
  let d = uiToDouble $ imageDiff i1 i2
  if n1 == n2
    then do
      putStrLn $ "same " ++ show d
      return (d:intraDiffs, interDiffs)
    else do
      putStrLn $ "diff " ++ show d
      return (intraDiffs, d:interDiffs)
  
main :: IO ()
main = do
  args <- getArgs
  let dir = head args
  putStrLn $ "numTests=" ++ show numTests
  files <- take numTests . drop 2 <$> readDirAndShuffle dir
  imgs <- mapM readImage2 files
  let imgPairs = [(a, b) | a <- imgs, b <- imgs, a /= b]
  (intraDiffs, interDiffs) <- foldM run ([], []) imgPairs
  putStrLn $ "Intra-cluster mean diff=" ++ show (mean intraDiffs)
  putStrLn $ "Intra-cluster std dev diff=" ++ show (popStdDev intraDiffs)
  putStrLn $ "Inter-cluster mean diff=" ++ show (mean interDiffs)
  putStrLn $ "Inter-cluster std dev diff=" ++ show (popStdDev interDiffs)
  putStrLn "test complete"
