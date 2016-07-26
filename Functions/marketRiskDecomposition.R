marketRiskDecomposition = function(R.cov, bm.name, p = NULL, annualized = F)
{
  source('Functions/betaFromCovMat.R')
  if(!is.null(p))
    z = qnorm(p)
  else
    z = 1
  if(is.xts(R.cov))
  {
    require(xts)
    require(stringr)
    varCols = which(lapply(str_split(names(R.cov), "_"), function(l)return(l[1]==l[2])) == T)
    R.vol = sqrt(R.cov[,varCols])
    names(R.vol) =  sapply(str_split(names(R.vol),"_"),function(l)return(l[1])) 
    
    #R.beta = apply(R.cov[,paste0(names(R.vol), "_", bm.name)], 2, function(c)return(xts(as.vector(c)/as.vector(R.cov[bm.name, bm.name]), order.by = index(R.cov)))) %>% tail()
    R.beta = betaFromCovMat(cov.mat = R.cov, bm.name = bm.name)
    
    R.vol_mkt = xts(t(sapply(1:nrow(R.cov), 
                             function(i)
                               return((as.vector(R.beta[i,])^2)*as.vector(R.cov[i,paste0(bm.name, "_", bm.name)])/as.vector(R.vol[i,]))
                             )
                      ), 
                    order.by = index(R.cov)
                    )
    names(R.vol_mkt) = names(R.vol)
    R.vol_spec = R.vol - R.vol_mkt
    result = lapply(names(R.vol[,-which(names(R.vol)==bm.name)]),
                    function(n)
                      {
                         res = cbind(R.vol[,n], R.vol_mkt[,n], R.vol_spec[, n])*z;
                         names(res) = c("TotalRisk", "MarketRisk", "SpecificRisk");
                         if(annualized)
                           res = sqrt(Frequency(res)[1])*res;
                         return(res)
                      }
                    )
    names(result) = names(R.vol[,-which(names(R.vol)==bm.name)])
    return(result)
  }
  else
  {
    R.vol = sapply(row.names(R.cov)[-which(row.names(R.cov)==bm.name)], function(n)return(sqrt(R.cov[n,n])))
    R.beta = betaFromCovMat(cov.mat = R.cov, bm.name = bm.name)
    R.vol_mkt = (R.beta^2)*R.cov[bm.name, bm.name]/R.vol
    R.vol_spec = R.vol - R.vol_mkt
    result = data.frame(TotalRisk = R.vol, MarketRisk = R.vol_mkt, SpecificRisk = R.vol_spec)*z
    if(annualized)
      result = sqrt(252)*result
    #row.names(result) = c("TotalRisk", "MarketRisk", "SpecificRisk")
    return(result)
  }
}
