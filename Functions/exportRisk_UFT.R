exportRisk_UFT = function(name, ufts, subs.o, weights, bms, width = 63)
{
    require(xts)
    require(PerformanceAnalytics)
    require(timeSeries)
#     width = 63
#     name = "LSE"
#     bms = cbind(sp2, eh)
#     weights = subs.weights
    
    actual = na.omit(ufts[,name])['200907/']
    wp = na.omit(weightedPortfolios(subs.o, subs.weights)[, name])
    
    id = switch(name, LSE = 785, LSD = 784, ED = 783, MN = 782)
    
    actual.alpha = rollingAlphaBeta(actual, bms, width = width)
    index(actual.alpha) = as.Date(index(actual.alpha))
    actual.vol = rollapply(actual, width = width, FUN = function(x)return(sqrt(252)*sd(x)))
    names(actual.vol) = paste(names(actual.vol), "Vol", sep="_")
    actual.dvol = rollapply(actual, width = width, FUN = function(x)return(sqrt(252)*DownsideDeviation(x)))
    names(actual.dvol) = paste(names(actual.dvol), "DVol", sep="_")
    actual.dd = as.xts(drawdowns(as.timeSeries(actual)))
    index(actual.dd) = as.Date(index(actual.dd))
    names(actual.dd) = paste(names(actual.dd), "DD", sep="_")
    actual.es = apply.rolling(actual, width = width, FUN = function(R)return(ES(R, p = 0.99, method = "gaussian")))
    names(actual.es) = paste(name, "ES", sep="_")
    actual.VaR = apply.rolling(actual, width = width, FUN = function(R)return(VaR(R, p = 0.99, method = "gaussian")))
    names(actual.VaR) = paste(name, "VaR", sep="_")
    actual.ir = rollingInformationRatio(actual, bms, width = width)
    index(actual.ir) = as.Date(index(actual.ir))
    actual.cor = RollingCorrelation(actual, bms, width = width)
    
    wp.alpha = rollingAlphaBeta(wp, bms, width = width)
    index(wp.alpha) = as.Date(index(wp.alpha))
    wp.vol = rollapply(wp, width = width, FUN = function(x)return(sqrt(252)*sd(x)))
    names(wp.vol) = paste(names(wp.vol), "Vol", sep="_")
    wp.dvol = rollapply(wp, width = width, FUN = function(x)return(sqrt(252)*DownsideDeviation(x)))
    names(wp.dvol) = paste(names(wp.dvol), "DVol", sep="_")
    wp.dd = as.xts(drawdowns(as.timeSeries(wp)))
    index(wp.dd) = as.Date(index(wp.dd))
    names(wp.dd) = paste(names(wp.dd), "DD", sep="_")
    wp.es = apply.rolling(wp, width = width, FUN = function(R)return(ES(R, p = 0.99, method = "gaussian")))
    names(wp.es) = paste(name, "ES", sep="_")
    wp.VaR = apply.rolling(wp, width = width, FUN = function(R)return(VaR(R, p = 0.99, method = "gaussian")))
    names(wp.VaR) = paste(name, "VaR", sep="_")
    wp.ir = rollingInformationRatio(wp, bms, width = width)
    index(wp.ir) = as.Date(index(wp.ir))
    wp.cor = RollingCorrelation(wp, bms, width = width)
    ##return()
    hist = cbind(actual.alpha
                              , actual.vol
                              , actual.dvol
                              , actual.dd
                              , actual.es
                              , actual.VaR
                              , actual.ir
                              , actual.cor)
    weightedPort = cbind( wp.alpha
                              , wp.vol
                              , wp.dvol
                              , wp.dd
                              , wp.es
                              , wp.VaR
                              , wp.ir
                              , wp.cor)
    names(hist) = paste0(names(hist), "_Actual")
    names(weightedPort) = paste0(names(weightedPort), "_CurrentPort")
    
    if(id == 777)
      data = cbind(hist, weightedPort)
    else
      data = cbind(sectorExposure(id, on = "days"), hist, weightedPort)
    
    exportXTS(data = data,
              filename = paste0("RISK_", name,  ".xlsx"),
              sheet = paste0("RISK_", name,  ".xlsx"))
}