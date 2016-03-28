library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

lse = subs.o$LSE[,1:5]
lse = lse[-which(rowSums(is.na(lse))==5),]
lse.full = lse[-which(rowSums(is.na(lse))>0),]
factors = sp.sectors[index(lse)]
lse.model = factorRoll(lse, factors, variable.selection = "subsets", window = 252, on = "days")

lse = lse.full[-c(1:2),]
lse = apply.weekly(lse, Return.cumulative)

fund.names = names(lse)
pspec = portfolio.spec(assets = fund.names)
pspec = add.constraint(portfolio = pspec, type = "full_investment")
pspec = add.constraint(portfolio = pspec, type = "long_only") 

minETL.95 = add.objective(portfolio = pspec, type = 'risk', name = 'ETL', arguments = list(p=0.95))
minETL.98 = add.objective(portfolio = pspec, type = 'risk', name = 'ETL', arguments = list(p=0.98))

minFactSD = add.objective(portfolio = pspec,  )



meanETL.95 = add.objective(portfolio = minETL.95, type = 'return', name = 'mean')
meanETL.98 = add.objective(portfolio = minETL.98, type = 'return', name = 'mean')

opt_minETL.95 = optimize.portfolio(R = lse, portfolio = minETL.95, optimize_method = "ROI", trace = T)
opt_minETL.98 = optimize.portfolio(R = lse, portfolio = minETL.98, optimize_method = "ROI", trace = T)
opt_minETL.95_r = optimize.portfolio(R = lse, portfolio = minETL.95, optimize_method = "random", trace = T, search_size = 10000)
opt_minETL.98_r = optimize.portfolio(R = lse, portfolio = minETL.98, optimize_method = "random", trace = T, search_size = 10000)



opt_meanETL.95 = optimize.portfolio(R = lse, portfolio = meanETL.95, optimize_method = "ROI", trace = T)
opt_meanETL.98 = optimize.portfolio(R = lse, portfolio = meanETL.98, optimize_method = "ROI", trace = T)

opt_meanETL.95_r = optimize.portfolio(R = lse, portfolio = meanETL.95, optimize_method = "random", trace = T, search_size = 10000)
opt_meanETL.98_r = optimize.portfolio(R = lse, portfolio = meanETL.98, optimize_method = "random", trace = T, search_size = 10000)