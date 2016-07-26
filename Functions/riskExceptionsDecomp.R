riskExceptionsDecomp = function(R, bm, p=0.99, asOfDate = NULL, rolling = F, lambda = 0.94)
{
  if(is.null(asOfDate))
    asOfDate = min(end(R), end(bm))
  
  R = R[paste0("/", asOfDate),]
  bm = bm[paste0("/", asOfDate)]
  
  if(!rolling)
  {
    R.cov = ewmaCovarianceMatrix_Full(rtn = cbind(R[-nrow(R),], bm[-nrow(bm),]), lambda = lambda)
    
    levels = marketRiskDecomposition(R.cov = R.cov, bm.name = names(bm)[1], p = p)
    R_asOf = R[asOfDate,]
    bm_asOf = bm[asOfDate,]
    decomp_AsOf = data.frame(t(sapply(names(R_asOf), 
           FUN = function(n)
              {
                beta_n = R.cov[n, names(bm)]/R.cov[names(bm), names(bm)];
                mkt_np1 = as.numeric(beta_n*bm_asOf);
                spec_np1 = as.numeric(R_asOf[, n] - mkt_np1);
                res = c(TotalReturn = as.numeric(R_asOf[,n]), MarketReturn = mkt_np1, SpecificReturn = spec_np1)
                return(res)
                
              }
           )))
    exceptions = data.frame(t(sapply(row.names(decomp_AsOf), 
                     function(n)
                       return(data.frame(TotalException = ifelse(decomp_AsOf[n,1] < -1*levels[n,1], decomp_AsOf[n,1],0),
                                         MarketException = ifelse(decomp_AsOf[n,2] < -1*levels[n,2], decomp_AsOf[n,2],0), 
                                         SpecificException = ifelse(decomp_AsOf[n,3] < -1*levels[n,3], decomp_AsOf[n,3],0)
                                         )))))
    return(list(Levels = levels,
                ReturnDecomp = decomp_AsOf,
                Exceptions = exceptions))
  }
  else
  {
    R.cov = ewmaCovariance_Full(rtn = cbind(R, bm), lambda = lambda)
    levels  = marketRiskDecomposition(R.cov = R.cov,
                                      bm.name = names(bm)[1],
                                      p = p
                                      )
    R.beta = betaFromCovMat(R.cov, bm.name = names(bm)[1])
    decomp = lapply(names(R), 
                    function(n)
                    {
                      TotalReturn_n = R[,n];
                      MarketReturn_n = lag(R.beta[,paste0(n, "_", names(bm)[1])])*bm;
                      SpecificReturn_n = TotalReturn_n - MarketReturn_n;
                      res = cbind(TotalReturn_n, MarketReturn_n, SpecificReturn_n)
                      names(res) = c("TotalReturn", "MarketReturn", "SpecificReturn");
                      return(res)
                    }
                    
                    
                    )
    names(decomp) = names(R)
    exceptions = lapply(names(R),
                        function(n)
                        {
                          res = cbind(ifelse(decomp[[n]][,"TotalReturn"]  < -1*levels[[n]][,"TotalRisk"], 
                                             decomp[[n]][,"TotalReturn"], 
                                             0),
                                      ifelse(decomp[[n]][,"MarketReturn"]  < -1*levels[[n]][,"MarketRisk"], 
                                             decomp[[n]][,"MarketReturn"], 
                                             0),
                                      ifelse(decomp[[n]][,"SpecificReturn"]  < -1*levels[[n]][,"SpecificRisk"], 
                                             decomp[[n]][,"SpecificReturn"], 
                                             0)
                                      );
                          names(res) = c("TotalException", "MarketException", "SpecificException");
                          return(na.omit(res))
                        }
                        )
    names(exceptions) = names(R)
    return(list(Levels = levels, ReturnDecomp = decomp, Exceptions = exceptions))
  }
}