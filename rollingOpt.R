years_start = seq.int(from = 1960, to = 2015, by = 1)
years_end = seq.int(from = 1961, to = 2016, by = 1)
years_start = as.character(years_start)
years_end = as.character(years_end)
opt_rolling = lapply(X=c(1:(length(years_end))), FUN = function(i)return(optRiskReturn(R = cbind(lse_predActual_dls_std, lse.m$Mkt.RF)[paste(years_start[i], years_end[i], sep = "/"),], fund.names = names(lse_predActual_dls_std), bm.name = "Mkt.RF", modName = "LS Regression", maxWeights = NULL, minWeights = NULL)))


df = data.frame( t(sapply(opt_rolling, function(x)return(x$OptimalWeights))))
colnames(df)[1] = colnames(opt_rolling)[1]

df = cbind(df, Mkt_Return = data.frame(sapply(opt_rolling, FUN = function(y)return(y$StatsTable["Annualized Return",7]))))
df = cbind(df, Mkt_Vol = data.frame(sapply(opt_rolling, FUN = function(y)return(y$StatsTable["Annualized Std Dev",7]))))
df = cbind(df, Mkt_DD = data.frame(sapply(opt_rolling, FUN = function(y)return(y$StatsTable["Maximum Drawdown",7]))))
df = cbind(df, Mkt_ES = data.frame(sapply(opt_rolling, FUN = function(y)return(y$StatsTable["Modified ES (95%)",7]))))

ncol(df)