options(java.parameters = "-Xmx4000m")
source('loadUp.R')

asOfDate = '20160711'
name = "SmithBreeden"


bms = getBenchmarks(getFundID(name))
bm.name = names(bms)[1]

asOf = paste0("/", asOfDate)
rets = na.omit(subs[asOf, name])

sub.stats = riskStats(rets)
sub.stats_rel = riskStats_relative(rets, width = 63, irWidth = 126)
sub.volContrib = reportVolatilityContribution(id = getSubID(name), name = name, endDate = asOfDate)

sub.stats_df = data.frame(Date = (as.numeric(index(sub.stats)) + 25569), sub.stats , row.names = NULL)
sub.stats_rel_df = data.frame(Date = (as.numeric(index(sub.stats_rel)) + 25569), sub.stats_rel , row.names = NULL)
sub.volContrib_df = lapply(sub.volContrib, FUN = function(a)return(
                          lapply(a, FUN = function(b)return(
                                  data.frame(Date = (as.numeric(index(b)) + 25569), 
                                                                              b , 
                                                                              row.names = NULL
                                                                              )
                                                             )
                           )))



library(XLConnect)
filename = "C:\\Users\\blloyd.HF\\Documents\\GitHub\\R-risk-mgmt\\Risk Reports\\Templates\\Risk_Template_CURRENT_TEST4.xlsm"
newfile = paste0("C:\\Users\\blloyd.HF\\Documents\\GitHub\\R-risk-mgmt\\Risk Reports\\Reports\\", asOfDate, "\\",
                 name, 
                 "_Risk_",
                 asOfDate,
                 ".xlsm")
                 

wb = loadWorkbook(filename = filename)

sheet = "RISKSTATS"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}

writeWorksheet(wb, data = sub.stats_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)

sheet = "RISKSTATS_RELATIVE"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}
writeWorksheet(wb, data = sub.stats_rel_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)

lapply(names(sub.volContrib_df), FUN = function(n)
  {
    sheet = paste0("EWMA_", toupper(n))
    lr = getLastRow(wb, sheet)
    lc = getLastColumn(wb, sheet)
    if(lr > 1)
    {
      clearRange(wb, sheet, coords = c(2, 1, lr, lc))
    }
    writeWorksheet(wb, data = sub.volContrib_df[[n]][["VolatilityContribution"]], sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)
    
    sheet = paste0("EWMA_EXPOSURE_", toupper(n))
    lr = getLastRow(wb, sheet)
    lc = getLastColumn(wb, sheet)
    if(lr > 1)
    {
      clearRange(wb, sheet, coords = c(2, 1, lr, lc))
    }
    writeWorksheet(wb, data = sub.volContrib_df[[n]][["WeightedExposure"]], sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)
    
  }
  )

writeWorksheet(wb, data = as.data.frame(name), sheet = "REPORT_NAME", startRow = 1, startCol = 1, header = F, rownames = F)
writeWorksheet(wb, data = as.data.frame(bm.name), sheet = "REPORT_NAME", startRow = 2, startCol = 1, header = F, rownames = F)

saveWorkbook(wb, file = newfile)




# exportXTS(
#   data = sub.stats, 
#   filename = "C:\\Users\\blloyd.HF\\Documents\\GitHub\\R-risk-mgmt\\Risk Reports\\Templates\\SUB_RISK_TEMPLATE_DATA.xlsx",
#   sheet = "RISKSTATS"
# )
# 
