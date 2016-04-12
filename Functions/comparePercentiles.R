comparePercentiles = function(data, p)
{
#   data = comp.bm
#   p = .25
  quantile.b = data[which(data[,3]<=quantile(data[,3], probs = c(p))),]
  quantile.u = data[which(data[,3]>=quantile(data[,3], probs = c(1-p))),]
  quantile.m = data[which((data[,3]<quantile(data[,3], probs = c(1-p))) & (data[,3]>quantile(data[,3], probs = c(p)))),]
  
  quantile.b.model = fitTsfm(asset.names = c("Historical", "NewPortfolio"), factor.names = names(data)[3], data = quantile.b)
  quantile.u.model = fitTsfm(asset.names = c("Historical", "NewPortfolio"), factor.names = names(data)[3], data = quantile.u)
  quantile.m.model = fitTsfm(asset.names = c("Historical", "NewPortfolio"), factor.names = names(data)[3], data = quantile.m)

  quantile.b.df = t(data.frame(Alpha = ((1+coef(quantile.b.model)[,1])^252 - 1), Beta = coef(quantile.b.model)[,2], r2 = quantile.b.model$r2, Cor = table.Correlation(quantile.b[,1:2], quantile.b[,3], digits = 3)[,1], N = rep(nrow(quantile.b),2)))
  row.names(quantile.b.df) = paste0(row.names(quantile.b.df), "_b")
  
  quantile.m.df = t(data.frame(Alpha = ((1+coef(quantile.m.model)[,1])^252 - 1), Beta = coef(quantile.m.model)[,2], r2 = quantile.m.model$r2, Cor = table.Correlation(quantile.m[,1:2], quantile.m[,3], digits = 3)[,1], N = rep(nrow(quantile.m),2)))
  row.names(quantile.m.df) = paste0(row.names(quantile.m.df), "_m")
  
  quantile.u.df = t(data.frame(Alpha = ((1+coef(quantile.u.model)[,1])^252 - 1), Beta = coef(quantile.u.model)[,2], r2 = quantile.u.model$r2, Cor = table.Correlation(quantile.u[,1:2], quantile.u[,3], digits = 3)[,1], N = rep(nrow(quantile.u),2)))
  row.names(quantile.u.df) = paste0(row.names(quantile.u.df), "_u")
  
  quantile.b.perf = table.AnnualizedReturns(quantile.b)[,1:2]
  row.names(quantile.b.perf)  = paste0(row.names(quantile.b.perf), "_b")
  quantile.b.perf[1:2,] = quantile.b.perf[1:2,]
  
  quantile.m.perf = table.AnnualizedReturns(quantile.m)[,1:2]
  row.names(quantile.m.perf)  = paste0(row.names(quantile.m.perf), "_m")
  quantile.m.perf[1:2,] = quantile.m.perf[1:2,]
  
  quantile.u.perf = table.AnnualizedReturns(quantile.u)[,1:2]
  row.names(quantile.u.perf)  = paste0(row.names(quantile.u.perf), "_u")
  quantile.u.perf[1:2,] = quantile.u.perf[1:2,]
  
  results = list(Upper = rbind(quantile.u.perf,quantile.u.df), Middle = rbind(quantile.m.perf,quantile.m.df), Bottom = rbind(quantile.b.perf,quantile.b.df))
  return(results)
}