riskStats = function(rets, width = 63)
{
  require(xts)
  require(PerformanceAnalytics)
  require(timeSeries)
  require(XLConnect)
  require(stringr)
  source('Functions/loadMyFuncs.R')
  loadMyFuncs()
  
  #      width = 63
  #      irWidth =126
  #      name = "Coe"
  #      sub = subs[,name]
  #      bms = cbind(sp2, hfrx$HFRXEH) 
  
  rets = na.omit(rets) 
  nm = names(rets)[1]
  
  lam = getLambda(nm)
  
  rets.bar = rets
  names(rets.bar) = "Daily_Return"
  
  rets.cum = cumulativeReturn(rets)
  names(rets.cum) = "Cumulative_Return"
  
  rets.vol = rollapply(rets, width = width, FUN = function(x)return(sqrt(252)*sd(x)))
  names(rets.vol) = "Volatility_63d"
  
  rets.dvol = rollapply(rets, width = width, FUN = function(x)return(sqrt(252)*DownsideDeviation(x)))
  names(rets.dvol) = "Downside_Vol"
  
  rets.dd = as.xts(drawdowns(as.timeSeries(rets)))
  index(rets.dd) = as.Date(index(rets.dd))
  names(rets.dd) = "Drawdown"
  
  rets.es = apply.rolling(rets, width = width, FUN = function(R)return(ES(R, p = 0.99, method = "gaussian")))
  names(rets.es) = "ETL"
  
  rets.VaR = apply.rolling(rets, width = width, FUN = function(R)return(VaR(R, p = 0.99, method = "gaussian")))
  names(rets.VaR) = "VaR"
  
  rets.ewma = ewmaVolatilityContribution(rets, lambda = lam)
  names(rets.ewma) = "EWMA"
  
  rets.cpts.meanVar = xts(ifelse(index(rets) %in% meanVarChangepoints(rets), 1, 0), order.by = index(rets))
  names(rets.cpts.meanVar) = c("MeanVar_Changepoints")
  
  rets.cpts.var =  xts(ifelse(index(rets) %in% varChangepoints(rets), 1, 0), order.by = index(rets))
  names(rets.cpts.var) = c("Vol_Changepoints")
  
  rets.stats =  cbind(rets.bar
                      , rets.cum
                      , rets.vol
                      , rets.ewma
                      , rets.dvol
                      , rets.dd
                      , rets.es
                      , rets.VaR
                      , rets.cpts.var
                      , rets.cpts.meanVar
                      
                    )
  return(rets.stats)
}



  
  


