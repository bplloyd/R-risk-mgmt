#IMPORT NAMESPACES
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(factorAnalytics)
library(quantmod)

#LOAD USER-DEFINED FUNCTIONS
source('Functions/apply.rolling2.R')

source('Functions/charts.AlphaBeta.R')
source('Functions/charts.PerformanceSummary2.R')

source('Functions/contributionSummary3.R')
source('Functions/compareLevels.R')
source('Functions/compareLevels2.R')
source('Functions/currentLevelsSubs.R')
source('Functions/createTimeSlices2.R')
source('Functions/currentLevelsSubs2.R')
source('Functions/currentLevelsFunds.R')
source('Functions/currentLevelsFunds2.R')
source('Functions/contributionBreakdown.R')
source('Functions/contributionBreakdown3.R')

source('Functions/dailyExceptions.R')
source('Functions/dailyVaRBounds.R')


source('Functions/extractSingleVaRroll.R')
source('Functions/extractSingleBetaroll.R')
source('Functions/extractFieldFromRoll.R')
source('Functions/executeSP.R')
source('Functions/ewmaModel.R')
source('Functions/ewmaCovariance.R')
source('Functions/extractAvgCorrelation.R')
source('Functions/exposureSummary.R')
source('Functions/exposureBreakdown.R')
source('Functions/exposureSummary2.R')
source('Functions/exposureBreakdown2.R')
source('Functions/exportXTS.R')

#source('Functions/getSpSectors.R')
source('Functions/getSpSectors2.R')
source('Functions/getSpReturns.R')
source('Functions/getFIReturns.R')
source('Functions/getHYReturns.R')
source('Functions/getMarketReturns.R')
source('Functions/getHFRX.R')
source('Functions/getAvgCor.R')
source('Functions/getAllocations.R')
source('Functions/getAllocations_Rolling.R')
source('Functions/getWeights.R')
source('Functions/getComponentRisk.R')
source('Functions/getQuandlKey.R')


source('Functions/loadUFTs.R')
source('Functions/loadPOFs.R')
source('Functions/loadIndices.R')
source('Functions/loadSubAdvisors.R')
source('Functions/loadSubAdvisor.R')
source('Functions/levelsForBarPlot.R')
source('Functions/loadCBOE.R')
source('Functions/loadH0A0.R')
source('Functions/loadMLFI.R')
source('Functions/loadTYields.R')
source('Functions/loadSubPort.R')




source('Functions/mVaR.R')

source('Functions/mVaRBounds.R')
source('Functions/nVaRBounds.R')
source('Functions/nVaR.R')

source('Functions/organizeExceptions.R')
source('Functions/organizeLevels.R')
source('Functions/organizeSubs.R')
source('Functions/organizeComponentRisks.R')

source('Functions/patchMF.R')

source('Functions/relativeVolatility.R')
source('Functions/reportComponentRisks.R')
source('Functions/reportAvgCorToBM.R')
source('Functions/rollAvgCorToBM.R')
source('Functions/rollingCorrelationMatrix.R')
source('Functions/rollCCTR.R')
source('Functions/rollTsfm2.R')
source('Functions/rollFmVaRdecomp.R')
source('Functions/rollFmESdecomp.R')
source('Functions/runTsFactorModel.R')
source('Functions/reportAvgCorrelation.R')
source('Functions/reportCorrelation.R')
source('Functions/riskStats.R')
source('Functions/riskStats_relative.R')
source('Functions/rollingStats.R')
source('Functions/rollingExposure.R')
source('Functions/rollingInformationRatio.R')

source('Functions/sectorExposure.R')

source('Functions/boxStats.R')
source('Functions/weightedPortfolios.R')
source('Functions/weightedAverageExposure.R')
source('Functions/volatilityContribution.R')
source('Functions/riskStats_sub.R')
source('Functions/riskStats_uft.R')
source('Functions/riskStats_SP5.R')
source('Functions/rollingAllocations.R')

source('Functions/alphaEWMAContribution_subs.R')
source('Functions/riskStats_Alpha.R')
source('Functions/pofAllocations.R')
source('Functions/pofAllocations_subs.R')
source('Functions/ewmaVolatilityContribution2.R')
#GET DATA OBJECTS
subs = loadSubAdvisors()
allocations = getAllocations(end(subs))
subs.o = organizeSubs(subs, curMgrs = T, allocations = allocations)
subs.weights = getWeights(subs.o)
#subs.weights$MF = subs.weights$MF[-which(names(subs.weights$MF)=="Centurion")]
#subs.weights$MF = subs.weights$MF/sum(subs.weights$MF)
ufts = patchMF(loadUFTs())
sp2 = getSpReturns2()
#pofs = loadPOFs()
inds = loadIndices()
mkt = getMarketReturns(inds)
sp = mkt$SPTR

hfrx = getHFRX(inds)

if(end(sp2) > end(sp)){sp = rbind(sp, sp2[which(index(sp2)>end(sp))])}
sp.sectors = getSpSectors2()

hy = getHYReturns(inds)
pofs = loadPOFs()

source('Functions/loadMyFuncs.R')
loadMyFuncs()

#source('Functions/nVaRExceptions.R')
#source('Functions/rollVaR.R')


# source('Functions/getFama3.R')
# source('Functions/currentVaRLevels.R')
# source('Functions/plotBounds.R')
# source('Functions/printBounds.R')
# source('Functions/cbind.lag.R')



