rollingStats = function(x, bm, width = 63)
{
    require(xts)
    require(timeSeries)
    require(PerformanceAnalytics)
  
    merged = na.omit(merge.xts(x, bm[index(x)]))
    bm = bm[paste(start(x), end(x), sep = "/")]
    vol = rollapply(x, width = width, FUN = function(x)return(sqrt(252)*sd(x)))
    dvol = rollapply(x, width = width, FUN = function(x)return(sqrt(252)*DownsideDeviation(x)))
    dd = drawdowns(as.timeSeries(x))
    beta = rollapply(data = na.omit(merged), width = width, FUN = function(x)return(lm(x[,1, drop = FALSE] ~ x[,2, drop = FALSE])$coefficients[2]), 
                     by = 1, by.column = FALSE, fill = T, align = "right")
    alpha = rollapply(data =na.omit(merged), width = width, FUN = function(x)return(lm(x[,1, drop = FALSE] ~ x[,2, drop = FALSE])$coefficients[1]), 
                      by = 1, by.column = FALSE, fill = T, align = "right")
    es.x = apply.rolling(x, width = width, FUN = function(R)return(ES(R, p = 0.99, method = "gaussian")))
    es.bm = apply.rolling(bm, width = width, FUN = function(R)return(ES(R, p = 0.99, method = "gaussian")))
    VaR.x = apply.rolling(x, width = width, FUN = function(R)return(VaR(R, p = 0.99, method = "gaussian")))
    VaR.bm = apply.rolling(bm, width = width, FUN = function(R)return(VaR(R, p = 0.99, method = "gaussian")))
    ap = rollCompare(x, bm, width = width, FUNC = "ActivePremium")
    te = rollCompare(x, bm, width = width, FUNC = "TrackingError")
    ir = ap/te
    res = cbind(vol, dvol, beta, alpha, dd, es.x, VaR.x, es.bm, VaR.bm, ap, te, ir)
    names(res) = c("vol", "downsidevol", "beta", "alpha", "drawdown", "ES", "VaR", "ES_bm", "VaR_bm", "ActivePremium", "TrackingError", "InformationRatio")
    return(as.xts(res))
}