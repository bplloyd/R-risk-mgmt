getComponentRisk = function(subs.o, subs.weights, p = 0.98, FUN = "VaR",  n = 126, reportDate = NULL, na.rm = T, method = NULL)
{
    require(PerformanceAnalytics)
    require(xts)
    if(is.null(method))
    {
       method = switch(FUN, VaR = "modified", ES = "modified", StdDev = "pearson")
    }
    
    f = match.fun(FUN)
    
    if(is.null(reportDate))
        reportDate = end(subs.o[[1]])
    
    result = vector(mode="list", length = length(subs.o))
    names(result) = names(subs.o)
    
    for (i in 1:length(subs.o)){
        weights = subs.weights[[which(names(subs.weights)==names(subs.o)[i])]]
        assets = subs.o[[i]][paste0("/", reportDate), names(weights)]
        assets = assets[(nrow(assets)-n+1):nrow(assets),]
        if((sum(rowSums(is.na(assets))) > 0) & (!na.rm))
            result[[i]] = NA
        else
          result[[i]] = f(R=na.omit(assets), weights = weights, p = p, method = method, portfolio_method = "component")
    }
    
    return(result)
}