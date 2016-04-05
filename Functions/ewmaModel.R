ewmaModel = function(data, fixed.pars = NULL)
{
  library(rugarch)
  if(is.null(fixed.pars))
    spec.ewma = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                       distribution.model="norm", fixed.pars=list(omega=0))
  if(!is.null(fixed.pars))
    spec.ewma = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                           mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                           distribution.model="norm", fixed.pars=fixed.pars) 
  fit = ugarchfit(data = na.omit(data), spec = spec.ewma)
  return(fit)
}



