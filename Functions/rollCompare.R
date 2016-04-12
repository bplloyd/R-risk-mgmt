rollCompare = function(x, bm, width=126, FUNC)
{
    x=na.omit(subs$Apis)
    bm = cbind(sp, ed)
    FUNC = "ActivePremium"
    FUNC = match.fun(FUNC)
    width = 126
    library(xts)
    library(PerformanceAnalytics)
    FUNC = match.fun(FUNC)
    slices = createTimeSlices2(x, initialWindow = width, fixedWindow = T, on = "days")
    res = lapply(slices, FUN = function(t)FUNC(x[t], bm[t,]))
    return(as.xts(unlist(lapply(slices, FUN = function(t)FUNC(x[t], bm[t])))))
}