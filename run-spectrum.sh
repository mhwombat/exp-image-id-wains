#!/bin/sh

# :l test/BigTest.hs
# mapM_ print [Params 60000 10000 False "/home/amy/mnist/trainingData/" "/home/amy/mnist/testData/" cr0X 0.0001 cdtX 900 0.1 0.00001 0.1 [-0.01, -0.01, -0.01, -0.01] depthX | cr0X <- [0.08,0.09,0.1,0.11], cdtX <- [0.08,0.11, 0.12, 0.13, 0.14, 0.15], depthX <- [1,2]]

# DO NOT DELETE
cd ~/numeral-wains/spectrum
# DO NOT DELETE


rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 8.0e-2, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 8.0e-2, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.11, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.11, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.12, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.12, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.13, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.13, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.14, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.14, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.15, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 8.0e-2, cDecay = 1.0e-4, cdt = 0.15, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 8.0e-2, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 8.0e-2, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.11, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.11, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.12, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.12, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.13, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.13, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.14, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.14, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.15, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 9.0e-2, cDecay = 1.0e-4, cdt = 0.15, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 8.0e-2, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 8.0e-2, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.11, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.11, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.12, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.12, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.13, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.13, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.14, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.14, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.15, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.1, cDecay = 1.0e-4, cdt = 0.15, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 8.0e-2, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 8.0e-2, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.11, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.11, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.12, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.12, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.13, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.13, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.14, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.14, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.15, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 1}' >> spectrum.log
rm -f superwain
mnist-big-test 'Params {nTrainingImages = 60000, nTestImages = 10000, reportNovelty = False, trainingDir = "/home/amy/mnist/trainingData/", testDir = "/home/amy/mnist/testData/", cr0 = 0.11, cDecay = 1.0e-4, cdt = 0.15, cSize = 900, pr0 = 0.1, pDecay = 1.0e-5, pdt = 0.1, defO = [-1.0e-2,-1.0e-2,-1.0e-2,-1.0e-2], depth = 2}' >> spectrum.log
