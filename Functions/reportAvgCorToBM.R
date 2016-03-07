reportAvgCorToBM = function(subs.o, bm, dates, width = 126)
{
    require(xts)
    require(magrittr)
    result =  t(as.data.frame(lapply(subs.o, FUN = function(x){x=as.xts(x); return(getAvgCor(x, bm, reportDate = dates[1], n=width))})))
    for(i in 2:length(dates))
        result = cbind(result, t(as.data.frame(lapply(subs.o, FUN = function(x){x=as.xts(x); return(getAvgCor(x, bm, reportDate = dates[i], n=width))}))))
    
    colnames(result) = as.character.Date(dates)
    return(result)
}