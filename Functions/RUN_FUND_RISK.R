options(java.parameters = "-Xmx4000m")
source('loadUp.R')
source('Functions/expectedVolContribution_fund.R')
source('Functions/lagMF.R')
source('Functions/getMFnames.R')


asOfDate = '20160711'
name = "HHSIX"
name.report = "HHSIX"
id = 774

bms = getBenchmarks(id)
bm.name = names(bms)[1]
lam = getLambda(name)
asOf = paste0("/", asOfDate)

if(name %in% c("ALPHX", "ALPIX", "HHSIX"))
{
  allocs = pofAllocations_subs(id)
  R = na.omit(pofs[asOf, name])
  allocs = allocs[asOf, ]
  
}
if(name %in% c("LSE", "LSD", "ED", "MF", "MN"))
{
  allocs = getAllocations_Rolling()
  allocs = allocs[[name]]
  R = na.omit(ufts[asOf, name])
  allocs = allocs[asOf, ]
}
R.subs = subs[asOf, names(allocs)]
if(id %in% c(786, 777))
  R.subs[, names(R.subs) %in% getMFnames()] = lagMF(R.subs[, names(R.subs) %in% getMFnames()])

allocs.cur = allocs[asOfDate, which(allocs[asOfDate]!=0)]
miscCols.allocs.cur = grep("Misc", names(allocs.cur))

allocs.cur = allocs.cur[, -c(miscCols.allocs.cur)]
allocs.cur = allocs.cur/sum(allocs.cur)


source('Functions/minVarPort.R')
source('Functions/weightedPortfolio.R')

R.currentPort = weightedPortfolio(R.subs, allocs.cur)


#MIN VARIANCE PORTFOLIOS
library(PortfolioAnalytics)
port.names = names(allocs.cur)
pspec = portfolio.spec(assets = port.names)
pspec = add.constraint(portfolio = pspec, type = "weight_sum", min_sum = 0.98, max_sum = 1.02)
pspec = add.constraint(portfolio = pspec, type = "long_only") 
risk.func = "StdDev"
risk.arguments = list()

maxWeights = rep(1, ncol(allocs.cur))
minWeights = rep(0, ncol(allocs.cur))
pspec = add.constraint(portfolio = pspec, type = "box", min = minWeights, max = maxWeights)
minRisk = add.objective(portfolio = pspec, type = 'risk', name = risk.func,arguments = risk.arguments )
opt_All = optimize.portfolio(R=R.subs[, port.names], portfolio = minRisk, optimize_method = "random", trace = T, search_size = 10000)
opt_252 = optimize.portfolio(R=R.subs[(nrow(R.subs)-251):nrow(R.subs), port.names], portfolio = minRisk, optimize_method = "random", trace = T, search_size = 10000)
opt_126 = optimize.portfolio(R=R.subs[(nrow(R.subs)-125):nrow(R.subs), port.names], portfolio = minRisk, optimize_method = "random", trace = T, search_size = 10000)
opt_63 = optimize.portfolio(R=R.subs[(nrow(R.subs)-62):nrow(R.subs), port.names], portfolio = minRisk, optimize_method = "random", trace = T, search_size = 10000)

weights.opt_All = weights(opt_All)
weights.opt_252 = weights(opt_252)
weights.opt_126 = weights(opt_126)
weights.opt_63 = weights(opt_63)

weights.opt_df = data.frame(rbind(weights.opt_63, weights.opt_126, weights.opt_252, weights.opt_All))
weights.opt_df = data.frame(Portfolio = c("63Day", "126Day", "252Day", "All"), weights.opt_df)

R.opt_All = weightedPortfolio(R.subs, weights.opt_All)
R.opt_252 = weightedPortfolio(R.subs, weights.opt_252)
R.opt_126 = weightedPortfolio(R.subs, weights.opt_126)
R.opt_63 = weightedPortfolio(R.subs, weights.opt_63)


R.opt_All_cum = cumprod(1+R.opt_All)-1
R.opt_252_cum  = cumprod(1+R.opt_252)-1
R.opt_126_cum  = cumprod(1+R.opt_126)-1
R.opt_63_cum  = cumprod(1+R.opt_63)-1
R.currentPort_cum = cumprod(1+R.currentPort) - 1

R.opt = cbind(R.currentPort, R.opt_63, R.opt_126, R.opt_252,R.opt_All )
R.opt_cum = cbind(R.currentPort_cum, R.opt_63_cum, R.opt_126_cum, R.opt_252_cum,R.opt_All_cum )

names(R.opt) = c("Current", "MinVar_63", "MinVar_126", "MinVar_252", "MinVar_All" )
names(R.opt_cum) = c("Current", "MinVar_63", "MinVar_126", "MinVar_252", "MinVar_All" )

subs.vol = ewmaVolatilityContribution(na.omit(R.subs[,1]), lambda = lam)
for(i in 2:ncol(R.subs))
{
  subs.vol = cbind(subs.vol, ewmaVolatilityContribution(na.omit(R.subs[,i]), lambda = lam))
}

R.volContrib_exp = expectedVolContribution_fund(id = id, subs = R.subs, lambda = lam, asOfDate = asOfDate, curWeightsOnly = F)
R.stats = riskStats(R)
R.stats_rel = riskStats_relative(R, width = 63, irWidth = 126)
R.volContrib = reportVolatilityContribution(id = id, name = name, endDate = asOfDate)


R.opt_df = data.frame(Date = (as.numeric(index(R.opt)) + 25569), R.opt , row.names = NULL)
R.opt_cum_df = data.frame(Date = (as.numeric(index(R.opt)) + 25569), R.opt_cum , row.names = NULL)
subs.vol_df = data.frame(Date = (as.numeric(index(subs.vol)) + 25569), subs.vol , row.names = NULL)
R.volContrib_exp_df = data.frame(Date = (as.numeric(index(R.volContrib_exp)) + 25569), R.volContrib_exp , row.names = NULL)
allocs_df = data.frame(Date = (as.numeric(index(allocs)) + 25569), allocs , row.names = NULL)
R.stats_df = data.frame(Date = (as.numeric(index(R.stats)) + 25569), R.stats , row.names = NULL)
R.stats_rel_df = data.frame(Date = (as.numeric(index(R.stats_rel)) + 25569), R.stats_rel , row.names = NULL)


R.volContrib_df = lapply(R.volContrib, FUN = function(a)return(
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


wb = loadWorkbook(filename = filename,create = F)


sheet = "MINVARPORTS_WEIGHTS"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}

writeWorksheet(wb, data = weights.opt_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)

sheet = "MINVARPORTS"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}

writeWorksheet(wb, data = R.opt_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)

sheet = "MINVARPORTS_CUM"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}

writeWorksheet(wb, data = R.opt_cum_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)

sheet = "ALLOCATIONS"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}
writeWorksheet(wb, data = allocs_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)



sheet = "EWMA_STANDALONE_SUBADVISOR"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}
writeWorksheet(wb, data = subs.vol_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)




sheet = "EWMA_EXPECTED_SUBADVISOR"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}
writeWorksheet(wb, data = R.volContrib_exp_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)



sheet = "RISKSTATS"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}
writeWorksheet(wb, data = R.stats_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)



sheet = "RISKSTATS_RELATIVE"
lr = getLastRow(wb, sheet)
lc = getLastColumn(wb, sheet)
if(lr > 1)
{
  clearRange(wb, sheet, coords = c(2, 1, lr, lc))
}
writeWorksheet(wb, data = R.stats_rel_df, sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)



lapply(names(R.volContrib_df), FUN = function(n)
{
  sheet = paste0("EWMA_", toupper(n))
  lr = getLastRow(wb, sheet)
  lc = getLastColumn(wb, sheet)
  if(lr > 1)
  {
    clearRange(wb, sheet, coords = c(2, 1, lr, lc))
  }
  writeWorksheet(wb, data = R.volContrib_df[[n]][["VolatilityContribution"]], sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)
  
  sheet = paste0("EWMA_EXPOSURE_", toupper(n))
  lr = getLastRow(wb, sheet)
  lc = getLastColumn(wb, sheet)
  if(lr > 1)
  {
    clearRange(wb, sheet, coords = c(2, 1, lr, lc))
  }
  writeWorksheet(wb, data = R.volContrib_df[[n]][["WeightedExposure"]], sheet = sheet, startRow = 1, startCol = 1,  header = T, rownames = F)
}
)


writeWorksheet(wb, data = as.data.frame(name.report), sheet = "REPORT_NAME", startRow = 1, startCol = 1, header = F, rownames = F)
writeWorksheet(wb, data = as.data.frame(bm.name), sheet = "REPORT_NAME", startRow = 2, startCol = 1, header = F, rownames = F)
saveWorkbook(wb, file = newfile)
