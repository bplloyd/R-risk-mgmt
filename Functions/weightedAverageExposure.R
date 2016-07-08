weightedAverageExposure = function(exposure, lambda, rolling = F)
{
  require(xts)
  
  exposure = na.fill(exposure, fill = 0)
#   allNa = which(rowSums(is.na(exposure))==ncol(exposure))
#   if(length(allNa)>0)
#   {
#     exposure
#   }
  if(!rolling)
  {
    nr = nrow(exposure)
    weights = (1-lambda)*lambda^((nr-1):0)
    norm.factor = 1/sum(weights)
    weights = norm.factor*weights
    return(apply(exposure, MARGIN = 2, FUN = function(c)return(c%*%weights)))
  }
  else
  {
    tslices = createTimeSlices2(data = exposure, initialWindow = 1, fixedWindow = F)
    res = xts(t(sapply(tslices, FUN = function(t){ weights = (1-lambda)*lambda^((length(t)-1):0);
                                                      norm.factor = 1/sum(weights);
                                                      weights = norm.factor * weights;
                                                      return(apply(exposure[t,], MARGIN = 2, FUN = function(c)return(c%*%weights)))
                                                    }
                          )
                    ),
              order.by = index(exposure)
              
                )
    index(res) = as.Date(index(res))
    return(res)
  }
}  

# n = 10
# lambda = 0.93
# weights = (1-lambda)*lambda^(0:n)
# norm.factor = 1/sum(weights)
# weights.norm = weights*norm.factor
# sum(weights.norm)
# 
# tslices = createTimeSlices2(exposure, initialWindow = 126, fixedWindow = F)
# wAvgExp.roll = t(sapply(tslices, FUN = function(t)return(weightedAverageExposure(exposure = exposure[t,], lambda = lambda))))