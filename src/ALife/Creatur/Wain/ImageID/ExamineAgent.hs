------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.ImageID.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Do a full analysis of a wain and generate a report.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Wain.ImageID.Experiment
import ALife.Creatur.Wain.Image.ExamineAgent (fetchWains, examine)
import System.Environment

main :: IO ()
main = do
  (f:_) <- getArgs
  ws <- fetchWains f :: IO [PatternWain]
  mapM_ examine ws
