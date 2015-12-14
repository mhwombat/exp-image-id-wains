#!/bin/bash

trainingDir=$HOME/mnist/trainingData
testDir=$HOME/mnist/testData
#trainingDir=$HOME/mnist/smallSet
#testDir=$HOME/mnist/smallSet

function runit {
  echo $*
  time  /home/eamybut/nosync/sandboxes/exp-image-id-wains/bin/exp-image-id-wains-single-test \
    $trainingDir $testDir $1 $2 $3 $4 $5 $6 > singleWain-$1-$2-$3-$4-$5-$6.log 2>&1
}

#runit 0.11 0.1 0.001 0.1 0.001 1
#runit 0.12 0.1 0.001 0.1 0.001 1
#runit 0.13 0.1 0.001 0.1 0.001 1
#runit 0.12 1 0.000000000000000001 1 0.000000000000000001 1
runit 0.12 1 0.000000000000000001 0.1 0.000000000000000001 1
