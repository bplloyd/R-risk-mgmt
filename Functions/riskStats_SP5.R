riskStats_SP5 = function(asOfDate, width = 63, exportToExcel = T)
{
    require(quantmod)
    require(TTR)
    require(Quandl)
    require(xts)
    require(PerformanceAnalytics)
    require(timeSeries)
    require(XLConnect)
    require(stringr)
    source('Functions/loadMyFuncs.R')
    loadMyFuncs()
    
    sp5 = getSymbols('^SP500TR', auto.assign = F)
    sp5_cl = sp5[,"SP500TR.Adjusted"]
    names(sp5_cl) = "SP500TR"
    sp5_r = na.omit(CalculateReturns(sp5_cl))
    
    Quandl.api_key(getQuandlKey())
    sp5_yahoo = Quandl(code = "YAHOO/INDEX_GSPC", type = "xts")
    sp5_yahoo = sp5_yahoo['1990/',]
    sp5_yahoo_r = na.omit(CalculateReturns(sp5_yahoo[, "Adjusted Close"])) 
    
    sp5_r = rbind(sp5_yahoo_r[1:(which(index(sp5_yahoo_r)==start(sp5_r))-1)], sp5_r)
    sp5_r = sp5_r[paste0("/",asOfDate),]
    
    cboe = loadCBOE()
    cboe = cboe[paste0("/",asOfDate),]
    
    lam = 0.93
    
  
    

    
    sp5.EMA = cbind(EMA(sp5_cl, 50),  EMA(sp5_cl, 100),EMA(sp5_cl, 200))
    names(sp5.EMA) = c("EMA_50", "EMA_100", "EMA_200")
    
    sp5.y_EMA = cbind(EMA(sp5_yahoo[, "Adjusted Close"], 50),  EMA(sp5_yahoo[, "Adjusted Close"], 100), EMA(sp5_yahoo[, "Adjusted Close"], 200))
    names(sp5.y_EMA) = c("EMA_50", "EMA_100", "EMA_200")
  
    
    sp5.stats =  as.data.frame(
                            cbind(
                                    sp5.bar
                                    , sp5.cum
                                    , sp5.vol
                                    , sp5.dvol
                                    , sp5.dd
                                    , sp5.es
                                    , sp5.VaR
                                    , sp5.cpts_var
                                    , sp5.cpts_meanVar
                                    , sp5.ewma
                                    , cboe[index(sp5_r), "VIX"]
                                    , cboe[index(sp5_r), "SKEW"]
                                    , cboe[index(sp5_r), "VXV"]
                                    , cboe[index(sp5_r), "VVIX"]
                                    , cboe[index(sp5_r), "SPX_PC"]
                                    , sp5.EMA[, "EMA_50"]
                                    , sp5.EMA[,"EMA_100"]
                                    , sp5.EMA[,"EMA_200"]
                                    , sp5.y_EMA[, "EMA_50"]
                                    , sp5.y_EMA[,"EMA_100"]
                                    , sp5.y_EMA[,"EMA_200"]
                                    , sp5_cl
                                  )
                          )

  
#     date63 = index(sp5_r[nrow(sp5_r)-62])
#     date252 = index(sp5_r[nrow(sp5_r)-251])
#     dateInception = start(na.omit(sp5_r))
#     
#     sp.boxStats = cbind(
#       boxStats(sp5_r[paste(date63, end(sp5_r), sep = "/")]),
#       boxStats(sp5_r[paste(date252, end(sp5_r), sep = "/")]),
#       boxStats(sp5_r[paste(dateInception, end(sp5_r), sep = "/")])
#     )
    
  
    
    
    if(exportToExcel)
    {
        df = data.frame(Rn = row.names(sp5.stats), sp5.stats, row.names = NULL)
        wb = loadWorkbook(filename = "MARKET_Risk_Template_TEST_OUT.xlsx", create = T)
        #wb = loadWorkbook(filename = "sp5_Risk_Template_TEST.xlsx", create = F)
        #writeWorksheet(object = wb, data = df, sheet = "RISKSTATS", startRow = 1, startCol = 2, header = T, rownames = F)
        createSheet(wb, name = "RISKSTATS")
        writeWorksheet(object = wb, data = df, sheet = "RISKSTATS", startRow = 1, startCol = 2, header = T, rownames = F)
        #createName(object = wb, name = "DATA_RANGE", formula = paste0("RISKSTATS!$A$2:$BP$", nrow(df)+1), overwrite = T)
        
        # writeNamedRegion(wb, data = as.data.frame(toupper(nm)), name = "REPORT_NAME", header = F, rownames = NULL)
        
#         writeNamedRegion(wb, data = box63, name = "BOX_RANGE_63", rownames = NULL, header = T)
#         writeNamedRegion(wb, data = box252, name = "BOX_RANGE_252", rownames = NULL, header = T)
#         writeNamedRegion(wb, data = boxInception, name = "BOX_RANGE_INCEPTION", rownames = NULL, header = T)
        
#         setForceFormulaRecalculation(wb, sheet = "RISKSTATS", value = T)
#         setForceFormulaRecalculation(wb, sheet = "BOXPLOTS", value = T)
#         #saveWorkbook(object=wb, file=paste0("RISK_DASHBOARD_", toupper(nm), ".xlsx"))
        saveWorkbook(object=wb)
        rm(wb)

    }
    return(sp5.stats)
}


