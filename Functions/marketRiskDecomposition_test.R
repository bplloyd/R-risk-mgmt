marketRiskDecomposition_test = function(R.cov, bm.name, p = NULL, annualized = F)
{
  
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
    result = lapply(index(R.vol),
                    function(d)
                      {
                         df = data.frame(TotalRisk = as.vector(R.vol[d,]), MarketRisk = as.vector(R.vol_mkt[d,]), SpecificRisk = as.vector(R.vol_spec[d,]))*z;
                         row.names(df) = names(R.vol);
                         if(annualized)
                           df = sqrt(252)*df;
                         return(df)
                      }
                    )
    names(result) = index(R.vol)
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
