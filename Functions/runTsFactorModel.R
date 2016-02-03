runTsFactorModel = function(assets, factors, startDate = NULL, endDate = NULL){
  require(factorAnalytics)
  require(xts)
  if(is.null(startDate))
      startDate = max(start(na.omit(factors)), start(na.omit(assets)))
  if(is.null(endDate))
      endDate  = min(end(na.omit(factors)), end(na.omit(assets)))
  assets.names = names(assets)
  factors.names = names(factors)
  modelData = merge.xts(assets[paste(startDate, endDate, sep = '/')], factors[paste(startDate, endDate, sep = '/')])
  return(fitTsfm(asset.names = assets.names, factor.names = factors.names, data = modelData, variable.selection = "stepwise"))
}