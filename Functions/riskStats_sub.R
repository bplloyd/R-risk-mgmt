riskStats_sub = function(sub, bms, width = 63, exportToExcel = T)
{
    require(xts)
    require(PerformanceAnalytics)
    require(timeSeries)
    require(XLConnect)
    source('Functions/loadMyFuncs.R')
    loadMyFuncs()
    
#      width = 63
#      name = "FrontFour"
#      sub = subs[,name]
#      bms = cbind(sp2, hfrx$HFRXED) 
     
    sub = na.omit(sub)  
    nm = names(sub)[1]
    id = getSubID(nm)
    fundId = getFundID(nm)
    lam = switch(as.character(fundId), '785' = 0.94, '784' = 0.91, '783' = 0.92, '782' = 0.95, '777' = 0.98)
    
    sub.bar = sub
    names(sub.bar) = "Daily Return"
    sub.cum = cumulativeReturn(sub)
    names(sub.cum) = "Cumulative Return"
    sub.alpha = rollingAlphaBeta(sub, bms, width = width)
    index(sub.alpha) = as.Date(index(sub.alpha))
    sub.vol = rollapply(sub, width = width, FUN = function(x)return(sqrt(252)*sd(x)))
    names(sub.vol) = "Volatility"
    sub.dvol = rollapply(sub, width = width, FUN = function(x)return(sqrt(252)*DownsideDeviation(x)))
    names(sub.dvol) = "Downside Volatility"
    sub.dd = as.xts(drawdowns(as.timeSeries(sub)))
    index(sub.dd) = as.Date(index(sub.dd))
    names(sub.dd) = "Drawdown"
    sub.es = apply.rolling(sub, width = width, FUN = function(R)return(ES(R, p = 0.99, method = "gaussian")))
    names(sub.es) = "ETL"
    sub.VaR = apply.rolling(sub, width = width, FUN = function(R)return(VaR(R, p = 0.99, method = "gaussian")))
    names(sub.VaR) = "VaR"
    sub.ir = rollingInformationRatio(sub, bms, width = width)
    index(sub.ir) = as.Date(index(sub.ir))
    sub.cor = rollingCorrelation(sub, bms, width = width)
    sub.ewma = sqrt(ewmaCovariance(sub, lambda = lam)*Frequency(sub))
    names(sub.ewma) = "EWMA Volatility"
    
    sub.cpts.meanVar = xts(ifelse(index(sub) %in% meanVarChangepoints(sub), 1, 0), order.by = index(sub))
    names(sub.cpts.meanVar) = c("Changepoints (meanvar)")
  
    sub.cpts.var =  xts(ifelse(index(sub) %in% varChangepoints(sub), 1, 0), order.by = index(sub))
    names(sub.cpts.var) = c("Changepoints (vol)")
    
    
    
    if((id != 33) & (id != 34) & (id != 105) & (id != 106))
    {
      sub.exp = rollingExposure(id)
      hist =  as.data.frame(cbind
                              (
                                  sub.bar
                                  , sub.cum
                                  , sub.alpha
                                  , sub.vol
                                  , sub.dvol
                                  , sub.dd
                                  , sub.es
                                  , sub.VaR
                                  , sub.ir
                                  , sub.cor
                                  , sub.cpts.var
                                  , sub.cpts.meanVar
                                  , sub.ewma
                                  , sectorExposure(id, on = "days")
                                  , sub.exp
                                )
                            )
      sub.tp = executeSP(procname = "usp_Top_Position_Sub", paramstring = paste0("@id = ", id))
      
      for(i in 2:ncol(sub.tp)){
        if(is.factor(sub.tp[,i])){
          sub.tp[,i] = as.character.factor(sub.tp[,i])
        }
      }
      
      sub.tp$DateReported = as.Date.factor(sub.tp$DateReported)
      sub.tp = data.frame(sub.tp[,2:ncol(sub.tp)], row.names = sub.tp$DateReported)
      hist = merge.data.frame(hist, sub.tp, by =0, all = T) 
    }
    else
    {
      hist =  as.data.frame(cbind
                              (
                                sub.bar
                                , sub.cum
                                , sub.alpha
                                , sub.vol
                                , sub.dvol
                                , sub.dd
                                , sub.es
                                , sub.VaR
                                , sub.ir
                                , sub.cor
                                , sub.cpts.var
                                , sub.cpts.meanVar
                                , sub.ewma
                              )
                            )
    }
    
#     exportXTS(data = data,
#               filename = paste0("RISKSTATS_", name,  ".xlsx"),
#               sheet = paste0("RISKSTATS_", name)
#               )
    if(exportToExcel)
    {
        df = data.frame(Date = row.names(hist), hist, row.names = NULL)
        writeWorksheetToFile(
                              file = paste0("RISKSTATS_", nm,  ".xlsx")
                              , sheet = paste0("RISKSTATS_", nm)
                              , data = df
                             )
    }
    
    return(hist)
}