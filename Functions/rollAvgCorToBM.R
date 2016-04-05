rollAvgCorToBM = function(portfolio, bm, width = 126, on = "days")
{
    require(xts)
    bm = xts(bm[paste(start(portfolio), end(portfolio), sep = "/"),], order.by = index(portfolio))
    tslices = createTimeSlices2(na.omit(portfolio), initialWindow = width, on = on)
    result = as.xts(unlist(lapply(tslices, function(t) return(getAvgCor(portfolio[t,], bm[t], n = width)))))
    names(result)[1] = paste0("AvgCor", width)
    return(result)
}