loadMyFuncs = function()
{
  source('Functions/apply.rolling2.R')
  source('Functions/executeSP.R')
  source('Functions/loadSubAdvisors.R')
  source('Functions/loadSubAdvisor.R')
  source('Functions/organizeSubs.R')
  source('Functions/loadUFTs.R')
  source('Functions/loadPOFs.R')
  source('Functions/loadIndices.R')
  source('Functions/nVaR.R')
  source('Functions/mVaR.R')
  source('Functions/nVaRBounds.R')
  source('Functions/mVaRBounds.R')
  source('Functions/compareLevels.R')
  source('Functions/compareLevels2.R')
  source('Functions/currentLevelsSubs.R')
  source('Functions/currentLevelsSubs2.R')
  source('Functions/currentLevelsFunds.R')
  source('Functions/currentLevelsFunds2.R')
  source('Functions/dailyExceptions.R')
  source('Functions/dailyVaRBounds.R')
  source('Functions/organizeExceptions.R')
  source('Functions/organizeLevels.R')
  source('Functions/getSpSectors.R')
  source('Functions/getSpReturns.R')
  source('Functions/getFIReturns.R')
  source('Functions/getHYReturns.R')
  source('Functions/getMarketReturns.R')

  source('Functions/rollCCTR.R')
  source('Functions/rollTsfm2.R')
  source('Functions/rollFmVaRdecomp.R')
  source('Functions/rollFmESdecomp.R')
  source('Functions/runTsFactorModel.R')
  source('Functions/extractSingleVaRroll.R')
  source('Functions/extractSingleBetaroll.R')
  source('Functions/extractFieldFromRoll.R')
  source('Functions/createTimeSlices2.R')
  source('Functions/getAllocations.R')
  source('Functions/getAllocations_Rolling.R')
  source('Functions/getWeights.R')
  source('Functions/getComponentRisk.R')
  source('Functions/organizeComponentRisks.R')
  source('Functions/reportComponentRisks.R')
  source('Functions/getAvgCor.R')
  source('Functions/reportAvgCorToBM.R')
  source('Functions/rollAvgCorToBM.R')
  source('Functions/rollingCorrelationMatrix.R')
  source('Functions/extractAvgCorrelation.R')
  source('Functions/reportAvgCorrelation.R')
  source('Functions/reportCorrelation.R')
  source('Functions/getQuandlKey.R')
  source('Functions/weightedPortfolios.R')
  source('Functions/levelsForBarPlot.R')
  source('Functions/ewmaModel.R')
  source('Functions/sectorExposure.R')
  source('Functions/rollingStats.R')
  source('Functions/charts.AlphaBeta.R')
  source('Functions/charts.PerformanceSummary2.R')
  source('Functions/exportXTS.R')
  source('Functions/rollingExposure.R')
  source('Functions/rollingAlphaBeta.R')
  source('Functions/getHFRX.R')
  source('Functions/cumulativeReturn.R')
  source('Functions/rollingInformationRatio.R')
  source('Functions/rollingCorrelation.R')
  source('Functions/meanVarChangepoints.R')
  source('Functions/ewmaCovariance.R')
  source('Functions/ewmaCovarianceMatrix.R')
  source('Functions/getSubID.R')
  source('Functions/getFundID.R')
  source('Functions/getLambda.R')
  source('Functions/ewmaVolatilityContribution.R')
  source('Functions/mu.geom.R')
  source('Functions/sd.geom.R')
  source('Functions/ewmaVolatilityContribution.R')
  source('Functions/geomIR.R')
  source('Functions/rollGeomIR.R')
  source('Functions/boxStats.R')
  source('Functions/loadCBOE.R')
  source('Functions/alphaEWMAContribution_subs.R')
  source('Functions/riskStats_Alpha.R')
  source('Functions/getBenchmarks.R')
  source('Functions/contributionSummary3.R')
  source('Functions/contributionBreakdown3.R')
  source('Functions/reportVolatilityContribution.R')
}