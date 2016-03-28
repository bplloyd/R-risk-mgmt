load('monthlyLpSMA.RData')
MonthlyInds <- read.csv("~/GitHub/R-risk-mgmt/MonthlyInds.csv", stringsAsFactors=FALSE)
bm.m = xts(MonthlyInds[,2:20], order.by = as.Date.character(MonthlyInds$Date, format = '%m/%d/%Y'))
bm.m = bm.m['2001/']
bm.m = xts(bm.m, order.by = index(lse.m.lp))
library(PerformanceAnalytics)
library(factorAnalytics)
lse.model_none = fitTsfm(asset.names = names(lse.m.lp), factor.names = names(bm.m), data = cbind(lse.m.lp, bm.m), variable.selection = "none")
lse.model_subsets  = fitTsfm(asset.names = names(lse.m.lp), factor.names = names(bm.m), data = cbind(lse.m.lp, bm.m), variable.selection = "subsets")
lse.model_stepwise  = fitTsfm(asset.names = names(lse.m.lp), factor.names = names(bm.m), data = cbind(lse.m.lp, bm.m), variable.selection = "stepwise")
factors_none = na.fill(coef(lse.model_none), fill = 0)
row.names(factors_none) = row.names(coef(lse.model_none))
factors_subsets = na.fill(coef(lse.model_subsets), fill = 0)
row.names(factors_subsets) = row.names(coef(lse.model_subsets))
factors_stepwise = na.fill(coef(lse.model_stepwise), fill = 0)
row.names(factors_stepwise) = row.names(coef(lse.model_stepwise))
variables = cbind(rep(1, nrow(bm.m)), bm.m)
names(variables)[1]="intercept"

lse.model_stepwise.fitted =  xts(variables %*% t(factors_stepwise), order.by = index(variables))
lse.model_none.fitted =  xts(variables %*% t(factors_none), order.by = index(variables))
lse.model_subsets.fitted =  xts(variables %*% t(factors_subsets), order.by = index(variables))

lse.m.lp.filled_none = lse.m.lp
lse.m.lp.filled_stepwise = lse.m.lp
lse.m.lp.filled_subsets = lse.m.lp

for(i in 1:ncol(lse.m.lp))
{
  naDates = index(lse.m.lp.filled_none[which(is.na(lse.m.lp.filled_none[,i])),])
  lse.m.lp.filled_none[naDates,i] = lse.model_none.fitted[naDates,i]
  lse.m.lp.filled_subsets[naDates,i] = lse.model_subsets.fitted[naDates,i]
  lse.m.lp.filled_stepwise[naDates,i] = lse.model_stepwise.fitted[naDates,i]
}


