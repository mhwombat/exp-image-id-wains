#!/bin/bash

trainingDir=$HOME/mnist/trainingData
testDir=$HOME/mnist/testData

function runit {
  echo $*
  time (exp-image-id-wains-single-test \
    $trainingDir $testDir $1 $2 $3 $4 $5 $6 $7) > singleWain-$1-$2-$3-$4-$5-$6-$7.log 2>&1
}

runit 0.10  0.1 0.0001      0.1 0.0001 1 64
runit 0.11  0.1 0.0001      0.1 0.0001 1 64
runit 0.115  0.1 0.0001      0.1 0.0001 1 64
runit 0.116  0.1 0.0001      0.1 0.0001 1 64
runit 0.117  0.1 0.0001      0.1 0.0001 1 64
runit 0.118  0.1 0.0001      0.1 0.0001 1 64
runit 0.119  0.1 0.0001      0.1 0.0001 1 64
runit 0.12  0.1 0.0001      0.1 0.0001 1 64
runit 0.13  0.1 0.0001      0.1 0.0001 1 64
runit 0.14  0.1 0.0001      0.1 0.0001 1 64
runit 0.15  0.1 0.0001      0.1 0.0001 1 64
runit 0.16  0.1 0.0001      0.1 0.0001 1 64
runit 0.17  0.1 0.0001      0.1 0.0001 1 64
runit 0.18  0.1 0.0001      0.1 0.0001 1 64
runit 0.19  0.1 0.0001      0.1 0.0001 1 64
runit 0.20  0.1 0.0001      0.1 0.0001 1 64

