rollTsfm3 = function(assets, factors, fit.method = "LS", variable.selection = "none", period = "6m", by = "1m"){
  require(factorAnalytics)
  require(fPortfolio)
  require(xts)
  minDate = min(as.Date.numeric(apply(assets, 2, function(x)return(start(na.omit((xts(x, order.by = index(assets)))))))))
  assets = assets[paste(minDate, "/", sep = ""),]
  factor.names = names(factors)
  windows = rollingWindows(assets, period = period, by = by)
  windows$from = windows$from
  windows$to = windows$to
  rollingModel = vector(mode = "list", length = length(windows$to))
  for(i in 1:length(windows$to)){
      from = windows$from[i]
      to = windows$to[i]
      asset.names = names(which(colSums(is.na(assets[paste(from, to,sep = "/"),]))==0))
      #factor.names = names(which(colSums(is.na(factors[paste(from, to,sep = "/"),]))==0))
      names(rollingModel)[i]= paste(to)
      rollingModel[[i]] = fitTsfm( 
                            asset.names = asset.names, factor.names = factor.names,    
                            data = cbind(assets[paste(from, to, sep = "/"), ],
                                          factors[paste(from, to, sep = "/"),]),
                            fit.method = fit.method,
                            variable.selection = variable.selection
                          )
  }
  return(rollingModel)
}