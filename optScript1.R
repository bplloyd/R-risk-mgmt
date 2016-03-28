library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)


# R = lse.m.lp.filled_none
# R= lse.m.lp.filled_subsets
R= lse.m.lp.filled_stepwise
#R = R[-c(1:2),]
#R = apply.weekly(R, Return.cumulative)
#factors = apply.weekly(sp.sectors, Return.cumulative)[-1,]
#factors = sp.sectors
#R.model = rollTsfm2(assets = R, factors = factors[index(R),], variable.selection = "none", window = nrow(R), on = "days")
#model = rollTsfm2(assets = R, factors = factors[index(R),], variable.selection = "none", window = nrow(R), on = "days")
#model = R.model[[length(R.model)]]

#betas = model$beta[names(R), names(factors)]
#res.sd = model$resid.sd[names(R)]

return.func = "mean"
return.arguments = list()

risk.func = "ETL"
risk.arguments = list(p=0.98)

fund.names = names(R)

pspec = portfolio.spec(assets = fund.names)
pspec = add.constraint(portfolio = pspec, type = "weight_sum", min_sum = 0.98, max_sum = 1.02)
pspec = add.constraint(portfolio = pspec, type = "long_only") 

minRisk = add.objective(portfolio = pspec, type = 'risk', name = risk.func,arguments = risk.arguments )
maxRiskRet = add.objective(portfolio = minRisk, type = "return", name = return.func, arguments = return.arguments)

opt_RiskRet_w = optimize.portfolio(R=R, portfolio = maxRiskRet, optimize_method = "DEoptim", trace = T)


# meanETL.95 = add.objective(portfolio = minETL.95, type = 'return', name = 'mean')
# meanETL.98 = add.objective(portfolio = minETL.98, type = 'return', name = 'mean')
# 
# opt_minETL.95 = optimize.portfolio(R = lse, portfolio = minETL.95, optimize_method = "ROI", trace = T)
# opt_minETL.98 = optimize.portfolio(R = lse, portfolio = minETL.98, optimize_method = "ROI", trace = T)
# opt_minETL.95_r = optimize.portfolio(R = lse, portfolio = minETL.95, optimize_method = "random", trace = T, search_size = 10000)
# opt_minETL.98_r = optimize.portfolio(R = lse, portfolio = minETL.98, optimize_method = "random", trace = T, search_size = 10000)
# 
# 
# 
# opt_meanETL.95 = optimize.portfolio(R = lse, portfolio = meanETL.95, optimize_method = "ROI", trace = T)
# opt_meanETL.98 = optimize.portfolio(R = lse, portfolio = meanETL.98, optimize_method = "ROI", trace = T)
# 
# opt_meanETL.95_r = optimize.portfolio(R = lse, portfolio = meanETL.95, optimize_method = "random", trace = T, search_size = 10000)
# opt_meanETL.98_r = optimize.portfolio(R = lse, portfolio = meanETL.98, optimize_method = "random", trace = T, search_size = 10000)