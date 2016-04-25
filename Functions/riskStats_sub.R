riskStats_sub = function(sub, bms, width = 63, exportToExcel = T)
{
    require(xts)
    require(PerformanceAnalytics)
    require(timeSeries)
    source('Functions/loadMyFuncs.R')
    
#     width = 63
#     name = "FrontFour"
#     sub = subs[,name]
    
    sub = na.omit(sub)  
    nm = names(sub)[1]
    id = getSubID(nm)
    fundId = getFundID(nm)
    lam = switch(as.character(fundId), '785' = 0.94, '784' = 0.91, '783' = 0.92, '782' = 0.95, '777' = 0.98)
    
    sub.cum = cumulativeReturn(sub)
    sub.alpha = rollingAlphaBeta(sub, bms, width = width)
    index(sub.alpha) = as.Date(index(sub.alpha))
    sub.vol = rollapply(sub, width = width, FUN = function(x)return(sqrt(252)*sd(x)))
    names(sub.vol) = paste(nm, "Vol", sep="_")
    sub.dvol = rollapply(sub, width = width, FUN = function(x)return(sqrt(252)*DownsideDeviation(x)))
    names(sub.dvol) = paste(nm, "DVol", sep="_")
    sub.dd = as.xts(drawdowns(as.timeSeries(sub)))
    index(sub.dd) = as.Date(index(sub.dd))
    names(sub.dd) = paste(nm, "DD", sep="_")
    sub.es = apply.rolling(sub, width = width, FUN = function(R)return(ES(R, p = 0.99, method = "gaussian")))
    names(sub.es) = paste(nm, "ES", sep="_")
    sub.VaR = apply.rolling(sub, width = width, FUN = function(R)return(VaR(R, p = 0.99, method = "gaussian")))
    names(sub.VaR) = paste(nm, "VaR", sep="_")
    sub.ir = rollingInformationRatio(sub, bms, width = width)
    index(sub.ir) = as.Date(index(sub.ir))
    sub.cor = rollingCorrelation(sub, bms, width = width)
    sub.cpts.meanVar = xts(ifelse(index(sub) %in% meanVarChangepoints(sub), 1, 0), order.by = index(sub))
    names(sub.cpts.meanVar) = c("Changepoints_meanvar")
  
    sub.cpts.var =  xts(ifelse(index(sub) %in% varChangepoints(sub), 1, 0), order.by = index(sub))
    names(sub.cpts.var) = c("Changepoints_var")
    
    hist = cbind(sub.cum, sub.alpha
                              , sub.vol
                              , sub.dvol
                              , sub.dd
                              , sub.es
                              , sub.VaR
                              , sub.ir
                              , sub.cor
                              , sub.cpts.var
                              , sub.cpts.meanVar)
    
    if((id != 33) & (id != 34) & (id != 105) & (id != 106))
    {
      sub.exp = rollingExposure(id)
      data = cbind(hist, sectorExposure(id, on = "days"), sub.exp)
     
    }
    else
    {
      data = cbind(hist)
    }
    
#     exportXTS(data = data,
#               filename = paste0("RISKSTATS_", name,  ".xlsx"),
#               sheet = paste0("RISKSTATS_", name)
#               )
    if(exportToExcel)
    {
          exportXTS(data = data,
                    filename = paste0("RISKSTATS_SUB", "_",nm, "_", end(sub),  ".xlsx"),
                    sheet = paste0("RISKSTATS_SUB", "_", nm) 
                  )
    }
    else
    {
      return(data)
    }
}