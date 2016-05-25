riskStats_pof = function(pof, bms, width = 63, irWidth = 126, exportToExcel = T)
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
    pof = na.omit(pofs$ALPHX) 
    bm1 = alpha.bm1
    bm2 = hfrx$HFRXGL
    bms = cbind(bm1, bm2)
    pof = na.omit(pof)['200907/', ]  
    
    endpof = end(pof)
    endBM1 = end(na.omit(bms['200907/',1]))
    endBM2 = end(na.omit(bms['200907/',2]))
    
    minEnd = min(endpof, endBM1, endBM2)
    pof = pof[paste0("/", minEnd)]
    bms = bms[paste0("/", minEnd), ]
    
    nm = names(pof)[1]
    id = switch(nm, LSE = 785, LSD = 784, ED = 783, MN = 782, MF = 777)
    fundId = getFundID(id)
    lam = 0.93
    #lam = switch(as.character(fundId), '785' = 0.93, '784' = 0.91, '783' = 0.92, '782' = 0.95, '777' = 0.98)
    
    
    ED.cbd_Sector = contributionBreakdown(783, "Sector", start = start(pof), end = end(pof))
    MN.cbd_Sector = contributionBreakdown(782, "Sector", start = start(pof), end = end(pof))
    LSD.cbd_Sector = contributionBreakdown(784, "Sector", start = start(pof), end = end(pof))
    LSE.cbd_Sector = contributionBreakdown(785, "Sector", start = start(pof), end = end(pof))
    
    
    
    
    MN.cbd_MktCap = contributionBreakdown(782, "Mkt_Cap", start = start(pof), end = end(pof))
    
    pof.ewma_volcontrib_Sector = ewmaVolatilityContribution(pof.cbd_Sector)
    pof.ewma_volcontrib_MktCap = ewmaVolatilityContribution(pof.cbd_MktCap)
   
    
    pof.cbd_SubAdvisor = contributionBreakdown(id, "SubAdvisor", start = start(pof), end = end(pof))
    pof.ewma_volcontrib_SubAdvisor = ewmaVolatilityContribution(pof.cbd_SubAdvisor)
    
    pof.bar = pof
    names(pof.bar) = "Daily Return"
    pof.cum = cumulativeReturn(pof)
    names(pof.cum) = "Cumulative Return"
    pof.alpha = rollingAlphaBeta(pof, bms, width = width)
    index(pof.alpha) = as.Date(index(pof.alpha))
    pof.vol = rollapply(pof, width = width, FUN = function(x)return(sqrt(252)*sd(x)))
    names(pof.vol) = "Volatility"
    pof.dvol = rollapply(pof, width = width, FUN = function(x)return(sqrt(252)*DownsideDeviation(x)))
    names(pof.dvol) = "Downside Volatility"
    pof.dd = as.xts(drawdowns(as.timeSeries(pof)))
    index(pof.dd) = as.Date(index(pof.dd))
    names(pof.dd) = "Drawdown"
    pof.es = apply.rolling(pof, width = width, FUN = function(R)return(ES(R, p = 0.99, method = "gaussian")))
    names(pof.es) = "ETL"
    pof.VaR = apply.rolling(pof, width = width, FUN = function(R)return(VaR(R, p = 0.99, method = "gaussian")))
    names(pof.VaR) = "VaR"
    pof.ir1 = rollGeomIR(pof, bms[,1], width = irWidth)
    #index(pof.ir1) = index(pof[paste(start(pof.ir1), end(pof.ir1), sep = "/")])
    pof.ir2 = rollGeomIR(pof, bms[,2], width = irWidth)
    pof.ir = cbind(pof.ir1, pof.ir2)
    index(pof.ir) = as.Date(index(pof.ir))
    index(pof.ir) = index(pof[index(pof.ir)])
    #index(pof.ir1) = as.Date(index(pof.ir))
    pof.cor = rollingCorrelation(pof, bms, width = width)
    pof.ewma = sqrt(ewmaCovariance(pof, lambda = lam)*Frequency(pof))
    names(pof.ewma) = "EWMA Volatility"
    
    pof.cpts.meanVar = xts(ifelse(index(pof) %in% meanVarChangepoints(pof), 1, 0), order.by = index(pof))
    names(pof.cpts.meanVar) = c("Changepoints (meanvar)")
  
    pof.cpts.var =  xts(ifelse(index(pof) %in% varChangepoints(pof), 1, 0), order.by = index(pof))
    names(pof.cpts.var) = c("Changepoints (vol)")
    
  
    if((id != 777) & (id!=784))
    {
      pof.exp = rollingExposure(id)
      pof.sectExp = sectorExposure(id, on = "days")
      hist =  as.data.frame(
        cbind
                              (
                                  pof.bar
                                  , pof.cum
                                  , pof.alpha
                                  , pof.vol
                                  , pof.dvol
                                  , pof.dd
                                  , pof.es
                                  , pof.VaR
                                  , pof.ir
                                  , pof.cor
                                  , pof.cpts.var
                                  , pof.cpts.meanVar
                                  , pof.ewma
                                  #, pof.sectExp
                                  #, pof.exp
                                  #, pof.ewma_volcontrib_Sector
                                  #, pof.ewma_volcontrib_MktCap
                                  #, pof.ewma_volcontrib_SubAdvisor
                                )['200907/', ]
                            )
      pof.tp = executeSP(procname = "usp_Top_Position_pof", paramstring = paste0("@id = ", id))
      
      for(i in 2:ncol(pof.tp)){
        if(is.factor(pof.tp[,i])){
          pof.tp[,i] = as.character.factor(pof.tp[,i])
        }
      }
      
      pof.tp$DateReported = as.Date.factor(pof.tp$DateReported)
      pof.tp = data.frame(pof.tp[,2:ncol(pof.tp)], row.names = pof.tp$DateReported)
      hist = merge.data.frame(hist['200907/',], pof.tp['200907/',], by =0, all = T) 
    }
    if(id==784)
    {
      pof.exp = rollingExposure(id)
      pof.sectExp = sectorExposure(id, on = "days")
      hist =  as.data.frame(
        cbind
        (
        pof.bar
        , pof.cum
        , pof.alpha
        , pof.vol
        , pof.dvol
        , pof.dd
        , pof.es
        , pof.VaR
        , pof.ir
        , pof.cor
        , pof.cpts.var
        , pof.cpts.meanVar
        , pof.ewma
        , pof.sectExp
        , pof.exp
        , pof.ewma_volcontrib_SP_Rating
        , pof.ewma_volcontrib_SubAdvisor
        )['200907/', ]
      )
      pof.tp = executeSP(procname = "usp_Top_Position_pof", paramstring = paste0("@id = ", id))
      
      for(i in 2:ncol(pof.tp)){
        if(is.factor(pof.tp[,i])){
          pof.tp[,i] = as.character.factor(pof.tp[,i])
        }
      }
      
      pof.tp$DateReported = as.Date.factor(pof.tp$DateReported)
      pof.tp = data.frame(pof.tp[,2:ncol(pof.tp)], row.names = pof.tp$DateReported)
      hist = merge.data.frame(hist['200907/',], pof.tp['200907/',], by =0, all = T) 
    }
    if(id==777)
    {
      hist =  as.data.frame(cbind
                              (
                                pof.bar
                                , pof.cum
                                , pof.alpha
                                , pof.vol
                                , pof.dvol
                                , pof.dd
                                , pof.es
                                , pof.VaR
                                , pof.ir
                                , pof.cor
                                , pof.cpts.var
                                , pof.cpts.meanVar
                                , pof.ewma
                                , pof.ewma_volcontrib_SubAdvisor
                              )['200907/', ]
                            )
    }
    
    date63 = index(pof[nrow(pof)-62])
    date252 = index(pof[nrow(pof)-252])
    dateInception = start(na.omit(pof))
    
    box63 = cbind(
      boxStats(pof[paste(date63, end(pof), sep = "/")]),
      boxStats(bms[paste(date63, end(pof), sep = "/"), 1]),
      boxStats(bms[paste(date63, end(pof), sep = "/"), 2])
    )
    
    box252 = cbind(
      boxStats(pof[paste(date252, end(pof), sep = "/")]),
      boxStats(bms[paste(date252, end(pof), sep = "/"), 1]),
      boxStats(bms[paste(date252, end(pof), sep = "/"), 2])
    )
    
    boxInception = cbind(
      boxStats(pof[paste(dateInception, end(pof), sep = "/")]),
      boxStats(bms[paste(dateInception, end(pof), sep = "/"), 1]),
      boxStats(bms[paste(dateInception, end(pof), sep = "/"), 2])
    )
    
    
    if(exportToExcel)
    {
        df = data.frame(Rn = row.names(hist), hist, row.names = NULL)
        wb = loadWorkbook(filename = "POF_Risk_Template_TEST_OUT.xlsx", create = T)
        #wb = loadWorkbook(filename = "POF_Risk_Template_TEST_OUT_ALPHA.xlsx", create = F)
        #writeWorksheet(object = wb, data = df, sheet = "RISKSTATS", startRow = 1, startCol = 2, header = T, rownames = F)
        writeWorksheet(object = wb, data = df, sheet = 1, startRow = 1, startCol = 2, header = T, rownames = F)
        createName(object = wb, name = "DATA_RANGE", formula = paste0("RISKSTATS!$A$2:$BP$", nrow(df)+1), overwrite = T)
        
        writeNamedRegion(wb, data = as.data.frame(toupper(nm)), name = "REPORT_NAME", header = F, rownames = NULL)
        
        writeNamedRegion(wb, data = box63, name = "BOX_RANGE_63", rownames = NULL, header = T)
        writeNamedRegion(wb, data = box252, name = "BOX_RANGE_252", rownames = NULL, header = T)
        writeNamedRegion(wb, data = boxInception, name = "BOX_RANGE_INCEPTION", rownames = NULL, header = T)
        
        setForceFormulaRecalculation(wb, sheet = "RISKSTATS", value = T)
        setForceFormulaRecalculation(wb, sheet = "BOXPLOTS", value = T)
        #saveWorkbook(object=wb, file=paste0("RISK_DASHBOARD_", toupper(nm), ".xlsx"))
        saveWorkbook(object=wb)
        rm(wb)

    }
    return(hist)
}


