rollingStats = function(x, bm, width = 63)
{
    require(xts)
    require(timeSeries)
    require(PerformanceAnalytics)
  
    merged = na.omit(merge.xts(x, bm))
    vol = rollapply(x, width = width, FUN = function(x)return(sqrt(252)*sd(x)))
    dvol = rollapply(x, width = width, FUN = function(x)return(sqrt(252)*DownsideDeviation(x)))
    dd = drawdowns(as.timeSeries(x))
    beta = rollapply(data = na.omit(merged), width = width, FUN = function(x)return(lm(x[,1, drop = FALSE] ~ x[,2, drop = FALSE])$coefficients[2]), 
                     by = 1, by.column = FALSE, fill = T, align = "right")
    alpha = rollapply(data =na.omit(merged), width = width, FUN = function(x)return(lm(x[,1, drop = FALSE] ~ x[,2, drop = FALSE])$coefficients[1]), 
                      by = 1, by.column = FALSE, fill = T, align = "right")
    res = cbind(vol, dvol, beta, alpha, dd)
    names(res) = c("vol", "downsidevol", "beta", "alpha", "drawdown")
    return(res)
}