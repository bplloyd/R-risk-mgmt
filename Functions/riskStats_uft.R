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
     
    uft = na.omit(uft)['200907/', ]  
    
    endUFT = end(uft)
    endBM1 = end(na.omit(bms['200907/',1]))
    endBM2 = end(na.omit(bms['200907/',2]))
    
    minEnd = min(endUFT, endBM1, endBM2)
    uft = uft[paste0("/", minEnd)]
    bms = bms[paste0("/", minEnd), ]
    
    nm = names(uft)[1]
    id = switch(nm, LSE = 785, LSD = 784, ED = 783, MN = 782, MF = 777)
    fundId = getFundID(id)
    lam = switch(as.character(fundId), '785' = 0.94, '784' = 0.91, '783' = 0.92, '782' = 0.95, '777' = 0.98)
    
    subs = loadSubAdvisors()
    subs = subs[paste0("/", minEnd), ]
    #subs = subs[paste0("/", minEnd),]
    allocs.subs = getAllocations_Rolling('2015-01-01', minEnd)[[nm]]
    
    
    standAlones = sqrt(252*ewmaCovariance(na.omit(subs[,names(allocs.subs)[1]])))
    
    
    for(i in 2:ncol(allocs.subs))
    {
      standAlones = cbind(standAlones, sqrt(252*ewmaCovariance(na.omit(subs[,names(allocs.subs)[i]]))))
    }
    names(standAlones) = names(allocs.subs)
    
    ewma.subs_curweights = ewmaVolatilityContribution2(R = subs[,names(allocs.subs)], weights = allocs.subs, lambda = lam, includeMisc = T, curWeightsOnly = T)
    ewma.subs_histweights = ewmaVolatilityContribution2(R = subs[,names(allocs.subs)], weights = allocs.subs, lambda = lam, includeMisc = T, curWeightsOnly = F)
    
    if((id != 777 ) & (id != 784))
    {
      uft.cbd_Sector = contributionBreakdown(id, "Sector", start = start(uft), end = end(uft))
      uft.cbd_MktCap = contributionBreakdown(id, "Mkt_Cap", start = start(uft), end = end(uft))
      uft.ewma_Sector = ewmaVolatilityContribution(uft.cbd_Sector,lambda = lam)
      uft.ewma_MktCap = ewmaVolatilityContribution(uft.cbd_MktCap,lambda = lam)
    }
    
    if(id == 784)
    {
      uft.ewma_AssetType = ewmaVolatilityContribution(rtn=contributionBreakdown(id, breakdown = "HAMF_TYPE", start = "2015-04-15", end = end(uft)), lambda = lam)
      uft.ewma_Rating = ewmaVolatilityContribution(rtn=contributionBreakdown(id, breakdown = "SP_Rating", start = "2015-04-15", end = end(uft)), lambda = lam)
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
    
    relVol = as.data.frame(relativeVolatility(Ra = uft, Rb = bms[,1], width = width))
    standAlones = as.data.frame(standAlones)
    allocs.subs = as.data.frame(allocs.subs)
  
    if((id != 777) & (id != 784))
    {
      uft.exp = rollingExposure(id)
      uft.sectExp = sectorExposure(id, on = "days")
      uft.stats =  as.data.frame(
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
                                  , uft.exp
                                  , uft.sectExp
                                )['200907/', ]
                            )
      
      
      
#       uft.tp = executeSP(procname = "usp_Top_Position_UFT", paramstring = paste0("@id = ", id))
#       
#       for(i in 2:ncol(uft.tp)){
#         if(is.factor(uft.tp[,i])){
#           uft.tp[,i] = as.character.factor(uft.tp[,i])
#         }
#       }
#       
#       uft.tp$DateReported = as.Date.factor(uft.tp$DateReported)
#       uft.tp = data.frame(uft.tp[,2:ncol(uft.tp)], row.names = uft.tp$DateReported)
#       uft.stats = merge.data.frame(uft.stats['200907/',], uft.tp['200907/',], by =0, all = T) 
    }
    if(id==784)
    {
      uft.exp = rollingExposure(id)
      uft.ratingExp = exposureBreakdown(id, breakdown = "SP_Rating", start = "2015-04-15", end = minEnd)
      uft.stats =  as.data.frame(
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
          , uft.exp
          , uft.ratingExp
#         , uft.ewma_volcontrib_SP_Rating
#         , uft.ewma_volcontrib_SubAdvisor
        )['200907/', ]
      )
#       uft.tp = executeSP(procname = "usp_Top_Position_UFT", paramstring = paste0("@id = ", id))
#       
#       for(i in 2:ncol(uft.tp)){
#         if(is.factor(uft.tp[,i])){
#           uft.tp[,i] = as.character.factor(uft.tp[,i])
#         }
#       }
#       
#       uft.tp$DateReported = as.Date.factor(uft.tp$DateReported)
#       uft.tp = data.frame(uft.tp[,2:ncol(uft.tp)], row.names = uft.tp$DateReported)
#       uft.stats = merge.data.frame(uft.stats['200907/',], uft.tp['200907/',], by =0, all = T) 
    }
    if(id==777)
    {
      uft.stats =  as.data.frame(cbind
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
                              )['200907/', ]
                            )
    }
    
    date63 = index(uft[nrow(uft)-62])
    date252 = index(uft[nrow(uft)-251])
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
        wb = loadWorkbook(filename = paste0(nm, "_Risk_Template_.xlsx"), create = T)
        if((id != 777) & (id != 784))
        {
          createSheet(wb, name = "EWMA_SECTOR")
          writeWorksheet(object = wb, data = as.data.frame(uft.ewma_Sector), sheet = "EWMA_SECTOR", startRow = 1, startCol = 1, header = T, rownames = "Date")
          createSheet(wb, name = "EWMA_MKTCAP")
          writeWorksheet(object = wb, data = as.data.frame(uft.ewma_MktCap), sheet = "EWMA_MKTCAP", startRow = 1, startCol = 1, header = T, rownames = "Date")
        }
        if(id == 784)
        {
          createSheet(wb, name = "EWMA_RATING")
          writeWorksheet(object = wb, data = as.data.frame(uft.ewma_Rating), sheet = "EWMA_RATING", startRow = 1, startCol = 1, header = T, rownames = "Date")
          createSheet(wb, name = "EWMA_ASSET_TYPE")
          writeWorksheet(object = wb, data = as.data.frame(uft.ewma_AssetType), sheet = "EWMA_ASSET_TYPE", startRow = 1, startCol = 1, header = T, rownames = "Date")
        }
        
          #         createSheet(wb, name = "EWMA_FUND")
#         writeWorksheet(object = wb, data = as.data.frame(ewma_Fund), sheet = "EWMA_FUND", startRow = 1, startCol = 1, header = T, rownames = "Date")
        createSheet(wb, name = "EWMA_SUBS_CUR")
        writeWorksheet(object = wb, data = as.data.frame(ewma.subs_curweights), sheet = "EWMA_SUBS_CUR", startRow = 1, startCol = 1, header = T, rownames = "Date")
        createSheet(wb, name = "EWMA_SUBS_HIST")
        writeWorksheet(object = wb, data = as.data.frame(ewma.subs_histweights), sheet = "EWMA_SUBS_HIST", startRow = 1, startCol = 1, header = T, rownames = "Date")
        
        createSheet(wb, name = "RISKSTATS")
        writeWorksheet(object = wb, data = uft.stats, sheet = "RISKSTATS", startRow = 1, startCol = 2, header = T, rownames = "Date")
        
        createSheet(wb, name = "RELVOL")
        writeWorksheet(object = wb, data = relVol, sheet = "RELVOL", startRow = 1, startCol = 1, header = T, rownames = "Date")
        
        createSheet(wb, name = "STANDALONE_VOL")
        writeWorksheet(object = wb, data = standAlones, sheet = "STANDALONE_VOL", startRow = 1, startCol = 1, header = T, rownames = "Date")
        
        createSheet(wb, name = "ALLOCATIONS")
        writeWorksheet(object = wb, data = allocs.subs, sheet = "ALLOCATIONS", startRow = 1, startCol = 1, header = T, rownames = "Date")
        
#         writeNamedRegion(wb, data = box63, name = "BOX_RANGE_63", rownames = NULL, header = T)
#         writeNamedRegion(wb, data = box252, name = "BOX_RANGE_252", rownames = NULL, header = T)
#         writeNamedRegion(wb, data = boxInception, name = "BOX_RANGE_INCEPTION", rownames = NULL, header = T)
        
        #         setForceFormulaRecalculation(wb, sheet = "RISKSTATS", value = T)
        #         setForceFormulaRecalculation(wb, sheet = "BOXPLOTS", value = T)
        saveWorkbook(object=wb)
        rm(wb)

    }
    return(uft.stats)
}


