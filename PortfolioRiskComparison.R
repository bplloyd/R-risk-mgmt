rm(comp, comp.bm)
library(ggplot2)
library(xts)
library(PerformanceAnalytics)
library(factorAnalytics)
library(data.table)

subs = loadSubAdvisors()
ufts = loadUFTs()
bm = sp2

sub.comps = cbind(subs$Apis,subs$Coe, subs$ISF)['2014/']
old.port = ufts$LSE['2014/']

comp= cbind(old.port, (1/3)*(sub.comps$Apis + sub.comps$Coe + sub.comps$ISF))
names(comp) = c("Historical","NewPortfolio")

comp.bm = cbind(comp, bm['2014/'])
comp.bm = comp.bm['/201603']
dt = as.data.table(comp.bm)
names(dt) = c("Date","Historical","NewPortfolio",names(bm)[1])
dt2 = melt.data.table(dt, id.vars = "Date", measure.vars = c("Historical","NewPortfolio", names(bm)[1]))
names(dt2) = c("Date", "Fund", "DailyReturn")

#

p=0.25

comp.bm.quantile.b = comp.bm[which(comp.bm[,3]<=quantile(comp.bm[,3], probs = c(p))),]
comp.bm.quantile.u = comp.bm[which(comp.bm[,3]>=quantile(comp.bm[,3], probs = c(1-p))),]
comp.bm.quantile.m = comp.bm[which((comp.bm[,3]<quantile(comp.bm[,3], probs = c(1-p))) & (comp.bm[,3]>quantile(comp.bm[,3], probs = c(p)))),]

comp.quantile.b.model = fitTsfm(asset.names = c("Historical", "NewPortfolio"), factor.names = names(bm)[1], data = comp.bm.quantile.b)
comp.quantile.u.model = fitTsfm(asset.names = c("Historical", "NewPortfolio"), factor.names = names(bm)[1], data = comp.bm.quantile.u)
comp.quantile.m.model = fitTsfm(asset.names = c("Historical", "NewPortfolio"), factor.names = names(bm)[1], data = comp.bm.quantile.m)

comp.quantile.b.model
comp.quantile.u.model
comp.quantile.m.model

comparePercentiles(comp.bm, p=0.25)


table.AnnualizedReturns(comp.bm.quantile.b)
table.AnnualizedReturns(comp.bm.quantile.u)
table.AnnualizedReturns(comp.bm.quantile.m)

table.Stats(comp.bm.quantile.b)
table.Stats(comp.bm.quantile.u)
table.Stats(comp.bm.quantile.m)

table.Distributions(comp.bm.quantile.b)
table.Distributions(comp.bm.quantile.u)
table.Distributions(comp.bm.quantile.m)
table.Distributions(comp.bm.quantile.u)


table.Correlation(comp.bm.quantile.b[,1:2],comp.bm.quantile.b[,3])
table.Correlation(comp.bm.quantile.u[,1:2],comp.bm.quantile.u[,3])

table.Correlation(comp.bm[,1:2], comp.bm[,3])
stats.hist_63 = rollingStats(comp.bm$Historical, comp.bm[,3],width = 63)
stats.new_63 = rollingStats(comp.bm$NewPortfolio, comp.bm[,3],width = 63)
stats.hist_126 = rollingStats(comp.bm$Historical, comp.bm[,3],width = 126)
stats.new_126 = rollingStats(comp.bm$NewPortfolio, comp.bm[,3],width = 126)

comp.es_126 = cbind(stats.hist_126$ES, stats.new_126$ES)
names(comp.es_126) = c("Historical", "NewPortfolio")

chart.Drawdown(comp, colorset = rich6equal, main = "Drawdown Comparison", legend.loc = "bottomleft")
shp = 3
ggplot(dt2, aes(y = DailyReturn, x = factor(Fund))) + geom_boxplot(width = 0.5, outlier.shape = shp, outlier.size = 5, notch = T, position = "dodge") + stat_summary(fun.y = "mean", geom = "point", shape = 4, size = 5, position = "dodge")


ggplot(dt2, aes(x = DailyReturn, fill = Fund)) + geom_density(alpha = 0.3)


apply.rolling(comp.bm[,1], width = 126, FUN = "StdDev.annualized")*100 -> sd.comp_hist
apply.rolling(comp.bm[,2], width = 126, FUN = "StdDev.annualized")*100 -> sd.comp_new
sd.comp_126 = cbind(sd.comp_hist, sd.comp_new)
names(comp.sd_126) = c("Historical", "NewPortfolio")

es.new_99 =  apply.rolling(comp.bm[,2], width = 126, FUN = function(R)return(ES(R, p=0.99, method = "gaussian")))
es.hist_99 = apply.rolling(comp.bm[,1], width = 126, FUN = function(R)return(ES(R, p=0.99, method = "gaussian")))
es.comp_99 = cbind(es.hist_99, es.new_99)
names(es.comp_99) = c("Historical", "NewPortfolio")


layout(matrix(c(1, 2)), widths = 1, heights = c(1,1))
par(mar = c(2, 2, 2, 2))
chart.TimeSeries(es.comp_99, legend.loc = "bottomleft", xaxis = F, colorset = rich6equal, main = "Rolling 126 Day Expected Tail Loss @ 99%")
par(mar = c(2, 2, 2, 2))
chart.TimeSeries(sd.comp_126, legend.loc = "bottomleft", xaxis = T, colorset = rich6equal, main = "Rolling 126 Day Std Deviation")


