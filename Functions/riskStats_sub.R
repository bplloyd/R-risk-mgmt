riskStats_sub = function(sub, bms, width = 63, irWidth = 126, exportToExcel = T)
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
     
    sub = na.omit(sub)  
    nm = names(sub)[1]
    id = getSubID(nm)
    fundId = getFundID(nm)
    lam = switch(as.character(fundId), '785' = 0.94, '784' = 0.91, '783' = 0.92, '782' = 0.95, '777' = 0.98)
    
    sub.cbd_Sector = contributionBreakdown(id, "Sector", start = start(sub), end = end(sub))
    sub.cbd_MktCap = contributionBreakdown(id, "Mkt_Cap", start = start(sub), end = end(sub))
    sub.ewma_volcontrib_Sector = ewmaVolatilityContribution(sub.cbd_Sector)
    sub.ewma_volcontrib_MktCap = ewmaVolatilityContribution(sub.cbd_MktCap)
    
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
    sub.ir1 = rollGeomIR(sub, bms[,1], width = irWidth)
    #index(sub.ir1) = index(sub[paste(start(sub.ir1), end(sub.ir1), sep = "/")])
    sub.ir2 = rollGeomIR(sub, bms[,2], width = irWidth)
    sub.ir = cbind(sub.ir1, sub.ir2)
    index(sub.ir) = index(sub[paste(start(sub.ir), end(sub.ir), sep = "/")])
    #index(sub.ir1) = as.Date(index(sub.ir))
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
      sub.sectExp = sectorExposure(id, on = "days")
      hist =  as.data.frame(
        cbind
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
                                  , sub.sectExp
                                  , sub.exp
                                  , sub.ewma_volcontrib_Sector
                                  , sub.ewma_volcontrib_MktCap
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
    if(exportToExcel)
    {
        df = data.frame(Rn = row.names(hist), hist, row.names = NULL)
        
        wb = loadWorkbook(filename = "Sub_Risk_Template_TEST.xlsx", create = F)
        writeWorksheet(object = wb, data = df, sheet = "RISKSTATS", startRow = 1, startCol = 2, header = T, rownames = F)
        createName(object = wb, name = "DATA_RANGE", formula = paste0("RISKSTATS!$A$2:$BP$", nrow(df)+1), overwrite = T)
        writeNamedRegion(wb, data = as.data.frame(toupper(nm)), name = "REPORT_NAME", header = F, rownames = NULL)
        setForceFormulaRecalculation(wb, sheet = "RISKSTATS", value = T)
        saveWorkbook(object=wb, file=paste0("RISK_DASHBOARD_", toupper(nm), ".xlsx"))
        rm(wb)

    }
    return(hist)
}


