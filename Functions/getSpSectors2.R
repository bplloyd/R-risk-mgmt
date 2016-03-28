getSpSectors2 = function(){
  require(xts)
  require(PerformanceAnalytics)
  require(Quandl)
  
  qkey = getQuandlKey()
  Quandl.api_key(qkey)
  secs = c("S5HLTH", "S5INFT", "S5INDU", "S5UTIL", "S5TELS", "S5MATR", "S5CONS", "S5COND", "SPN", "SPF")
  secs = paste("SPDJ", secs, sep = "/")
  res = Quandl(code = secs, type = "xts")
  res = res[, c(1,3,5,7,9,11,13,15,17,19)]
  names(res) = secs
  res = CalculateReturns(res)[-1,]
  
  
  
}