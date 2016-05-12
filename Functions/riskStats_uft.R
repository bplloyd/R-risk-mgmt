riskStats_uft = function(uft, bms, width = 63, irWidth = 126, exportToExcel = T)
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
#      uft = ufts[,name]
#      bms = cbind(sp2, hfrx$HFRXEH) 
     
    uft = na.omit(uft)  
    
    endUFT = end(uft)
    endBM1 = end(na.omit(bms[,1]))
    endBM2 = end(na.omit(bms[,2]))
    
    minEnd = min(endUFT, endBM1, endBM2)
    uft = uft[paste0("/", minEnd)]
    bms = bms[paste0("/", minEnd), ]
    
    nm = names(uft)[1]
    id = switch(nm, LSE = 785, LSD = 784, ED = 783, MN = 782, MF = 777)
    fundId = getFundID(id)
    lam = switch(as.character(fundId), '785' = 0.94, '784' = 0.91, '783' = 0.92, '782' = 0.95, '777' = 0.98)
    
    if(id!=777)
    {
      uft.cbd_Sector = contributionBreakdown(id, "Sector", start = start(uft), end = end(uft))
      uft.cbd_MktCap = contributionBreakdown(id, "Mkt_Cap", start = start(uft), end = end(uft))
      uft.ewma_volcontrib_Sector = ewmaVolatilityContribution(uft.cbd_Sector)
      uft.ewma_volcontrib_MktCap = ewmaVolatilityContribution(uft.cbd_MktCap)
    }
    
    uft.cbd_SubAdvisor = contributionBreakdown(id, "SubAdvisor", start = start(uft), end = end(uft))
    uft.ewma_volcontrib_SubAdvisor = ewmaVolatilityContribution(uft.cbd_SubAdvisor)
    
    uft.bar = uft
    names(uft.bar) = "Daily Return"
    uft.cum = cumulativeReturn(uft)
    names(uft.cum) = "Cumulative Return"
    uft.alpha = rollingAlphaBeta(uft, bms, width = width)
    index(uft.alpha) = as.Date(index(uft.alpha))
    uft.vol = rollapply(uft, width = width, FUN = function(x)return(sqrt(252)*sd(x)))
    names(uft.vol) = "Volatility"
    uft.dvol = rollapply(uft, width = width, FUN = function(x)return(sqrt(252)*DownsideDeviation(x)))
    names(uft.dvol) = "Downside Volatility"
    uft.dd = as.xts(drawdowns(as.timeSeries(uft)))
    index(uft.dd) = as.Date(index(uft.dd))
    names(uft.dd) = "Drawdown"
    uft.es = apply.rolling(uft, width = width, FUN = function(R)return(ES(R, p = 0.99, method = "gaussian")))
    names(uft.es) = "ETL"
    uft.VaR = apply.rolling(uft, width = width, FUN = function(R)return(VaR(R, p = 0.99, method = "gaussian")))
    names(uft.VaR) = "VaR"
    uft.ir1 = rollGeomIR(uft, bms[,1], width = irWidth)
    #index(uft.ir1) = index(uft[paste(start(uft.ir1), end(uft.ir1), sep = "/")])
    uft.ir2 = rollGeomIR(uft, bms[,2], width = irWidth)
    uft.ir = cbind(uft.ir1, uft.ir2)
    index(uft.ir) = as.Date(index(uft.ir))
    index(uft.ir) = index(uft[index(uft.ir)])
    #index(uft.ir1) = as.Date(index(uft.ir))
    uft.cor = rollingCorrelation(uft, bms, width = width)
    uft.ewma = sqrt(ewmaCovariance(uft, lambda = lam)*Frequency(uft))
    names(uft.ewma) = "EWMA Volatility"
    
    uft.cpts.meanVar = xts(ifelse(index(uft) %in% meanVarChangepoints(uft), 1, 0), order.by = index(uft))
    names(uft.cpts.meanVar) = c("Changepoints (meanvar)")
  
    uft.cpts.var =  xts(ifelse(index(uft) %in% varChangepoints(uft), 1, 0), order.by = index(uft))
    names(uft.cpts.var) = c("Changepoints (vol)")
    
  
    if((id != 777) & (id!=784))
    {
      uft.exp = rollingExposure(id)
      uft.sectExp = sectorExposure(id, on = "days")
      hist =  as.data.frame(
        cbind
                              (
                                  uft.bar
                                  , uft.cum
                                  , uft.alpha
                                  , uft.vol
                                  , uft.dvol
                                  , uft.dd
                                  , uft.es
                                  , uft.VaR
                                  , uft.ir
                                  , uft.cor
                                  , uft.cpts.var
                                  , uft.cpts.meanVar
                                  , uft.ewma
                                  , uft.sectExp
                                  , uft.exp
                                  , uft.ewma_volcontrib_Sector
                                  , uft.ewma_volcontrib_MktCap
                                  , uft.ewma_volcontrib_SubAdvisor
                                )
                            )
      uft.tp = executeSP(procname = "usp_Top_Position_UFT", paramstring = paste0("@id = ", id))
      
      for(i in 2:ncol(uft.tp)){
        if(is.factor(uft.tp[,i])){
          uft.tp[,i] = as.character.factor(uft.tp[,i])
        }
      }
      
      uft.tp$DateReported = as.Date.factor(uft.tp$DateReported)
      uft.tp = data.frame(uft.tp[,2:ncol(uft.tp)], row.names = uft.tp$DateReported)
      hist = merge.data.frame(hist, uft.tp, by =0, all = T) 
    }
    if(id==784)
    {
      uft.exp = rollingExposure(id)
      uft.sectExp = sectorExposure(id, on = "days")
      hist =  as.data.frame(
        cbind
        (
        uft.bar
        , uft.cum
        , uft.alpha
        , uft.vol
        , uft.dvol
        , uft.dd
        , uft.es
        , uft.VaR
        , uft.ir
        , uft.cor
        , uft.cpts.var
        , uft.cpts.meanVar
        , uft.ewma
        , uft.sectExp
        , uft.exp
        , uft.ewma_volcontrib_SP_Rating
        , uft.ewma_volcontrib_SubAdvisor
        )
      )
      uft.tp = executeSP(procname = "usp_Top_Position_UFT", paramstring = paste0("@id = ", id))
      
      for(i in 2:ncol(uft.tp)){
        if(is.factor(uft.tp[,i])){
          uft.tp[,i] = as.character.factor(uft.tp[,i])
        }
      }
      
      uft.tp$DateReported = as.Date.factor(uft.tp$DateReported)
      uft.tp = data.frame(uft.tp[,2:ncol(uft.tp)], row.names = uft.tp$DateReported)
      hist = merge.data.frame(hist, uft.tp, by =0, all = T) 
    }
    if(id==777)
    {
      hist =  as.data.frame(cbind
                              (
                                uft.bar
                                , uft.cum
                                , uft.alpha
                                , uft.vol
                                , uft.dvol
                                , uft.dd
                                , uft.es
                                , uft.VaR
                                , uft.ir
                                , uft.cor
                                , uft.cpts.var
                                , uft.cpts.meanVar
                                , uft.ewma
                                , uft.ewma_volcontrib_SubAdvisor
                              )
                            )
    }
    
    date63 = index(uft[nrow(uft)-62])
    date252 = index(uft[nrow(uft)-252])
    dateInception = start(na.omit(uft))
    
    box63 = cbind(
      boxStats(uft[paste(date63, end(uft), sep = "/")]),
      boxStats(bms[paste(date63, end(uft), sep = "/"), 1]),
      boxStats(bms[paste(date63, end(uft), sep = "/"), 2])
    )
    
    box252 = cbind(
      boxStats(uft[paste(date252, end(uft), sep = "/")]),
      boxStats(bms[paste(date252, end(uft), sep = "/"), 1]),
      boxStats(bms[paste(date252, end(uft), sep = "/"), 2])
    )
    
    boxInception = cbind(
      boxStats(uft[paste(dateInception, end(uft), sep = "/")]),
      boxStats(bms[paste(dateInception, end(uft), sep = "/"), 1]),
      boxStats(bms[paste(dateInception, end(uft), sep = "/"), 2])
    )
    
    
    if(exportToExcel)
    {
        df = data.frame(Rn = row.names(hist), hist, row.names = NULL)
        
        wb = loadWorkbook(filename = "UFT_Risk_Template_TEST.xlsx", create = F)
        writeWorksheet(object = wb, data = df, sheet = "RISKSTATS", startRow = 1, startCol = 2, header = T, rownames = F)
        createName(object = wb, name = "DATA_RANGE", formula = paste0("RISKSTATS!$A$2:$BP$", nrow(df)+1), overwrite = T)
        
        writeNamedRegion(wb, data = as.data.frame(toupper(nm)), name = "REPORT_NAME", header = F, rownames = NULL)
        
        writeNamedRegion(wb, data = box63, name = "BOX_RANGE_63", rownames = NULL, header = T)
        writeNamedRegion(wb, data = box252, name = "BOX_RANGE_252", rownames = NULL, header = T)
        writeNamedRegion(wb, data = boxInception, name = "BOX_RANGE_INCEPTION", rownames = NULL, header = T)
        
        setForceFormulaRecalculation(wb, sheet = "RISKSTATS", value = T)
        setForceFormulaRecalculation(wb, sheet = "BOXPLOTS", value = T)
        
        saveWorkbook(object=wb, file=paste0("RISK_DASHBOARD_", toupper(nm), ".xlsx"))
        rm(wb)

    }
    return(hist)
}


