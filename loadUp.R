#IMPORT NAMESPACES
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(factorAnalytics)

#LOAD USER-DEFINED FUNCTIONS
source('Functions/apply.rolling2.R')
source('Functions/executeSP.R')
source('Functions/loadSubAdvisors.R')
source('Functions/organizeSubs.R')
source('Functions/loadUFTs.R')
source('Functions/loadPOFs.R')
source('Functions/loadIndices.R')
source('Functions/nVaR.R')
source('Functions/mVaR.R')
source('Functions/nVaRBounds.R')
source('Functions/mVaRBounds.R')
source('Functions/compareLevels.R')
source('Functions/currentLevelsSubs.R')
source('Functions/currentLevelsFunds.R')
source('Functions/dailyExceptions.R')
source('Functions/dailyVaRBounds.R')
source('Functions/organizeExceptions.R')
source('Functions/organizeLevels.R')
source('Functions/getSpSectors.R')
source('Functions/getSpReturns.R')
source('Functions/rollCCTR.R')
source('Functions/rollTsfm.R')
source('Functions/rollFmVaRdecomp.R')
source('Functions/runTsFactorModel.R')
source('Functions/extractSingleVaRroll.R')
source('Functions/extractSingleBetaroll.R')
source('Functions/extractFieldFromRoll.R')
source('Functions/createTimeSlices2.R')
source('Functions/getAllocations.R')
source('Functions/getWeights.R')
#GET DATA OBJECTS
subs = loadSubAdvisors()
subs.o = organizeSubs(subs)
ufts = loadUFTs()
pofs = loadPOFs()
inds = loadIndices()


#source('Functions/nVaRExceptions.R')
#source('Functions/rollVaR.R')


# source('Functions/getFama3.R')
# source('Functions/currentVaRLevels.R')
# source('Functions/plotBounds.R')
# source('Functions/printBounds.R')
# source('Functions/cbind.lag.R')



