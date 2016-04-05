sp5 <- read.csv("~/GitHub/R-risk-mgmt/sp5.csv", stringsAsFactors=FALSE)
r3k <- read.csv("~/GitHub/R-risk-mgmt/r3k.csv", stringsAsFactors=FALSE)
r2k <- read.csv("~/GitHub/R-risk-mgmt/r2k.csv", stringsAsFactors=FALSE)
r1k <- read.csv("~/GitHub/R-risk-mgmt/r1k.csv", stringsAsFactors=FALSE)
hfrx_daily <- read.csv("~/GitHub/R-risk-mgmt/hfrx_daily.csv", stringsAsFactors=FALSE)
sp5 = toXts(x = sp5, name = "sptr")
r1k = toXts(x = r1k, name = "r1000")
r2k = toXts(x = r2k, name = "r2000")
r3k = toXts(x = r3k, name = "r3000")
eq = merge.xts(sp5, r1k, r2k, r3k)

hfrx = data.table(hfrx_daily)
hfrx.cast = dcast.data.table(hfrx, formula = Date ~ Ticker, value.var = "DailyROR", fun.aggregate = mean)
hfrx.cast = hfrx.cast[-c(1:2)]
hfrx.cast$Date = as.Date.character(hfrx.cast$Date, format = "%m/%d/%Y")
setkey(hfrx.cast, Date)
hfrx.cast[,c("V1") := NULL]
hfrx.xts = as.xts(hfrx.cast)/100