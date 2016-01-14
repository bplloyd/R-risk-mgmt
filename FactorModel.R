sql = "SELECT CAST(v.DateReported AS date) 'DateReported', v.Ticker, v.PX_LAST FROM v_Index_Px_Daily AS v"
index.result = sqlQuery(cn, sql) %>% melt(., id.vars = 1:2) %>% dcast(., DateReported ~ Ticker, max)
as.xts(index.result[,names(index.result)[-1]], order.by = as.Date.factor(index.result$DateReported) ) -> inds
inds['2009/'] -> inds
CalculateReturns(inds)->inds
library(factorAnalytics)
merge.xts(ufts, inds)->myData
assets = names(myData)[1:5]
facs = names(myData)[6:57]
myData.train = myData['2011/2015-06']
myData.test = myData['2015-07/']
fitTsfm(asset.names = assets, factor.names = facs, data = myData.train, variable.selection = "stepwise")->ufts.Model
lse.Model = ufts.Model$asset.fit$LSE
mn.Model = ufts.Model$asset.fit$MN
ed.Model = ufts.Model$asset.fit$ED
lsd.Model = ufts.Model$asset.fit$LSD
mf.Model = ufts.Model$asset.fit$MF
ufts.pred = predict(ufts.Model, newdata = myData.test)
ufts.pred$LSE %>% xts(, order.by = index(myData.test)) -> lse.pred
ufts.pred$LSD %>% xts(, order.by = index(myData.test)) -> lsd.pred
ufts.pred$ED %>% xts(, order.by = index(myData.test)) -> ed.pred
ufts.pred$MN %>% xts(, order.by = index(myData.test)) -> mn.pred
ufts.pred$MF %>% xts(, order.by = index(myData.test)) -> mf.pred
merge.xts(myData.test$LSE, lse.pred) -> lse.predVSact
merge.xts(myData.test$LSD, lsd.pred)-> lsd.predVSact
merge.xts(myData.test$ED, ed.pred)-> ed.predVSact
merge.xts(myData.test$MF, mf.pred)-> mf.predVSact
merge.xts(myData.test$MN, mn.pred)-> mn.predVSact
chart.CumReturns(predVsAct, legend.loc = "bottom", main = "LSE Factor Model (Russ + SP sectors) vs Actual Returns - Model Fit from 2011 to June 2015")


