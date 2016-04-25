rollingAlphaBeta = function(Ra, Rb, width = 63)
{
  source('Functions/rollFit.R')
  require(xts)
  require(PerformanceAnalytics)
  fit = rollFit(na.omit(cbind(Ra, Rb[,1])), formula = paste(names(Ra), names(Rb[,1]), sep = " ~ "), width = width)
  res = as.xts(t(sapply(fit, FUN = function(x)
                          return(cbind(alpha = t(coef(x))[1], 
                                        beta = t(coef(x))[2], 
                                        r2=summary(x)$r.squared)
                                 )
                         )
                  )
                )
  names(res) = c("alpha", "beta", "r2")
  res$alpha = (1+res$alpha)^Frequency(Ra) - 1
  names(res) = paste(names(res), names(Rb)[1], sep = "_")
  
  if(ncol(Rb)>1){
    for(i in 2:ncol(Rb)){
      fit = rollFit(na.omit(cbind(Ra, Rb[,i])), formula = paste(names(Ra), names(Rb[,i]), sep = " ~ "), width = width)
      res = cbind(res, as.xts(t(sapply(fit, FUN = function(x)
        return(cbind(alpha = t(coef(x))[1], 
                     beta = t(coef(x))[2], 
                     r2=summary(x)$r.squared)
        )
      )
      )
      )
      )
      names(res)[(3*(i-1)+1):(3*i)] = c("alpha", "beta", "r2")
      res$alpha = (1+res$alpha)^Frequency(Ra) - 1
      names(res)[(3*(i-1)+1):(3*i)] = paste(names(res)[(3*(i-1)+1):(3*i)], names(Rb)[i], sep = "_")
    }
  }
  return(res)
}