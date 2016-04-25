library(PerformanceAnalytics)
library(xts)
library(data.table)


HAMF_lps = read.csv(file = "MonthlyLPs.csv", header = T, stringsAsFactors = F)
INDS.m = read.csv(file = "MonthlyInds.csv", header = T, stringsAsFactors = F)
inds.m = xts(INDS.m[,2:ncol(INDS.m)], order.by = as.Date.character(INDS.m$Date, format = "%m/%d/%Y"))
lps = xts(HAMF_lps[,2:ncol(HAMF_lps)], order.by = index(inds.m[-nrow(inds.m),]))


nms = c("Apis", "Coe","FrontFour","Havens", "ISF", "Jadwin", "Revolution", "Row", "SmithBreeden", "Soundpoint", "Longbow")  
subs2 = res[,nms]
subs.m = apply.monthly(subs2, Return.cumulative)
#sma_lp = cbind(subs.m, lps)[,1:ncol(subs.m)]

for(i in 1:ncol(subs.m)){
  subs.m[which(subs.m[,i]==0),i]=NA
  subs.m[start(na.omit(subs.m[,i])),i]=NA
}
subs.m = subs.m[-which(rowSums(is.na(subs.m))==ncol(subs.m)),]
subs.m = subs.m[-nrow(subs.m),]
index(subs.m[-nrow(subs.m),]) = index(lps[paste0(start(subs.m),"/"),])

sma_lp = xts(cbind(subs.m, lps)[, 1:ncol(subs.m)])
for(i in 1:ncol(sma_lp)){
    startRow = which(index(sma_lp)==start(na.omit(sma_lp[,i]))) - 1
    colNumLP = grep(names(sma_lp)[i], names(lps))
    sma_lp[1:startRow, i] = lps[1:startRow,colNumLP]
}



i=i+1
head(na.omit(subs2[,i]))
head(na.omit(subs.m2[,i]))



subs.m2[start(na.omit(subs.m2[,i])),i]=NA
head(na.omit(subs.m2[,i]))
