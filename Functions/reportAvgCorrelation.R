reportAvgCorrelation = function(dates, subs.o = NULL, n = 126, avgCor = NULL)
{
    require(xts)
    require(magrittr)
    if(is.null(avgCor))
    {
        if(is.null(subs.o))
            subs.o = organizeSubs(loadSubAdvisors())
        
        avgCor = lapply(subs.o, FUN = function(x)return(extractAvgCorrelation(rollingCorrelationMatrix(x, window = n))))
    }
         
    result =  t(as.data.frame(lapply(avgCor, FUN = function(x)return(x[dates[1]]))))
    for(i in 2:length(dates))
        result = cbind(result, t(as.data.frame(lapply(avgCor, FUN = function(x)return(x[dates[i]])))))

    return(result)
}