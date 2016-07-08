riskStats_Alpha = function(pof, bms, width = 63, irWidth = 126, exportToExcel = T)
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
    #pof = na.omit(pofs$ALPHX) 
    
  
    pof = na.omit(pof)['200907/', ]  
    id = 786
#     endpof = end(pof)
#     endBM1 = end(na.omit(bms['200907/',1]))
#     endBM2 = end(na.omit(bms['200907/',2]))
#     
#     minEnd = min(endpof, endBM1, endBM2)
#     pof = pof[paste0("/", minEnd)]
#     bms = bms[paste0("/", minEnd), ]
    
    nm = names(pof)[1]
    lam = 0.93
    subs = loadSubAdvisors()
    ufts = loadUFTs()
    #subs = subs[paste0("/", minEnd),]
    
    
    mf.names = c("Centurion", "Dominion", "MiscMF", "Revolution", "Row")
    subs.mf = subs[, which(names(subs) %in% mf.names)] 
    subs.mf = lag(subs.mf, -1)
   
    allocs.subs = pofAllocations_subs(786, startDate = "2014-01-01", endDate = "2016-06-15", includeMisc = T)
    
    # allocs.subs = pofAllocations_subs(786, startDate = "2014-01-01", endDate = end(pof), includeMisc = T)
    subs.alpha = cbind(subs.mf, subs[, -which(names(subs) %in% mf.names)])[, names(allocs.subs)] 
    subs.alpha = subs.alpha[-nrow(subs.alpha),]
    
    maxDate = min(end(subs.alpha), end(allocs.subs))
    subs.alpha= subs.alpha[paste0("/", maxDate),]
    allocs.subs=allocs.subs[paste0("/", maxDate),]
    
    
    
    standAlones_SUBS = sqrt(252*ewmaCovariance(na.omit(subs[,names(allocs.subs)[1]])))
    
    
    for(i in 2:ncol(allocs.subs))
    {
      standAlones_SUBS = cbind(standAlones_SUBS, sqrt(252*ewmaCovariance(na.omit(subs[,names(allocs.subs)[i]]))))
    }
    names(standAlones_SUBS) = names(allocs.subs)
    
    for(i in 1:ncol(standAlones_SUBS))
    {
      if(names(standAlones_SUBS)[i] %in% mf.names)
      {
        standAlones_SUBS[,i] = lag(standAlones_SUBS[,i],-1)
      }
    }
    
    standAlones_UFTS = sqrt(252*ewmaCovariance(na.omit(ufts[,1])))
    for(i in 2:ncol(ufts))
    {
      standAlones_UFTS = cbind(standAlones_UFTS, sqrt(252*ewmaCovariance(na.omit(ufts[,i]))))
    }
    names(standAlones_UFTS) = names(ufts)
    standAlones_UFTS$MF = lag(standAlones_UFTS$MF, -1)
    
    standAlones = cbind(standAlones_SUBS, standAlones_UFTS)
    standAlones = standAlones[-nrow(standAlones),]
    
    ewma.subs_curweights = ewmaVolatilityContribution2(R = subs.alpha, weights = allocs.subs, lambda = 0.93, includeMisc = T, curWeightsOnly = T)
    ewma.subs_histweights = ewmaVolatilityContribution2(R = subs.alpha, weights = allocs.subs, lambda = 0.93, includeMisc = T, curWeightsOnly = F)
    ewma.subs_histweights_noMisc = ewmaVolatilityContribution2(R = subs, weights = allocs.subs, lambda = 0.93, includeMisc = F, curWeightsOnly = F)
    
    ewma_Sector = ewmaVolatilityContribution(rtn=contributionBreakdown(786,breakdown = "Sector", start = "2015-04-15", end =  end(pof)), lambda = lam)
    ewma_AssetType = ewmaVolatilityContribution(rtn=contributionBreakdown(786,breakdown = "HAMF_TYPE", start = "2015-04-15", end = end(pof)), lambda = lam)
    ewma_Fund = ewmaVolatilityContribution(rtn=contributionBreakdown(786,breakdown = "Fund_UID", start = "2015-04-15", end =  end(pof)), lambda = lam)
    
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
    
    pof.ir2 = rollGeomIR(pof, bms[,2], width = irWidth)
    pof.ir = cbind(pof.ir1, pof.ir2)
    index(pof.ir) = as.Date(index(pof.ir))
    index(pof.ir) = index(pof[index(pof.ir)])
   
    pof.cor = rollingCorrelation(pof, bms, width = width)
    pof.ewma = sqrt(ewmaCovariance(pof, lambda = lam)*Frequency(pof))
    names(pof.ewma) = "EWMA Volatility"
    
    pof.cpts.meanVar = xts(ifelse(index(pof) %in% meanVarChangepoints(pof), 1, 0), order.by = index(pof))
    names(pof.cpts.meanVar) = c("Changepoints (meanvar)")
  
    pof.cpts.var =  xts(ifelse(index(pof) %in% varChangepoints(pof), 1, 0), order.by = index(pof))
    names(pof.cpts.var) = c("Changepoints (vol)")
    

    pof.exp = rollingExposure(id)
    pof.sectExp = sectorExposure(id, on = "days")
    rollingStats =  as.data.frame(
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
                                  , bms[,1]
                                  , bms[,2]
                                )['200907/', ]
                            )
    
    relVol = as.data.frame(relativeVolatility(Ra = pof, Rb = bms[,1], width = width))
    standAlones = as.data.frame(standAlones)
    allocs.subs = as.data.frame(allocs.subs)
    
    
    date63 = index(pof[nrow(pof)-62])
    date252 = index(pof[nrow(pof)-251])
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
        wb = loadWorkbook(filename = "POF_Risk_Template_TEST2.xlsx", create = F)
        createSheet(wb, name = "EWMA_SECTOR")
        writeWorksheet(object = wb, data = as.data.frame(ewma_Sector), sheet = "EWMA_SECTOR", startRow = 1, startCol = 1, header = T, rownames = "Date")
        createSheet(wb, name = "EWMA_ASSET_TYPE")
        writeWorksheet(object = wb, data = as.data.frame(ewma_AssetType), sheet = "EWMA_ASSET_TYPE", startRow = 1, startCol = 1, header = T, rownames = "Date")
        createSheet(wb, name = "EWMA_FUND")
        writeWorksheet(object = wb, data = as.data.frame(ewma_Fund), sheet = "EWMA_FUND", startRow = 1, startCol = 1, header = T, rownames = "Date")
        createSheet(wb, name = "EWMA_SUBS_CUR")
        writeWorksheet(object = wb, data = as.data.frame(ewma.subs_curweights), sheet = "EWMA_SUBS_CUR", startRow = 1, startCol = 1, header = T, rownames = "Date")
        createSheet(wb, name = "EWMA_SUBS_HIST")
        writeWorksheet(object = wb, data = as.data.frame(ewma.subs_histweights), sheet = "EWMA_SUBS_HIST", startRow = 1, startCol = 1, header = T, rownames = "Date")
        
        writeWorksheet(object = wb, data = rollingStats, sheet = "RISKSTATS", startRow = 1, startCol = 2, header = T, rownames = "Date")
        writeWorksheet(object = wb, data = relVol, sheet = "RELVOL", startRow = 1, startCol = 1, header = T, rownames = "Date")
        writeWorksheet(object = wb, data = standAlones, sheet = "STANDALONE_VOL", startRow = 1, startCol = 1, header = T, rownames = "Date")
        writeWorksheet(object = wb, data = allocs.subs, sheet = "ALLOCATIONS", startRow = 1, startCol = 1, header = T, rownames = "Date")
        
        writeNamedRegion(wb, data = box63, name = "BOX_RANGE_63", rownames = NULL, header = T)
        writeNamedRegion(wb, data = box252, name = "BOX_RANGE_252", rownames = NULL, header = T)
        writeNamedRegion(wb, data = boxInception, name = "BOX_RANGE_INCEPTION", rownames = NULL, header = T)
        
#         setForceFormulaRecalculation(wb, sheet = "RISKSTATS", value = T)
#         setForceFormulaRecalculation(wb, sheet = "BOXPLOTS", value = T)
        saveWorkbook(object=wb)
        rm(wb)
    }
}

# wb = loadWorkbook(filename = "Template_TEST.xlsx", create = T)
# createSheet(wb, name = "TEST")
# writeWorksheet(object = wb, data = rollingStats, sheet = "TEST", startRow = 1, startCol = 2, header = T, rownames = "Date")
# saveWorkbook(object=wb)
