getSp5_QuandlYahoo = function()
{
    require(xts)
    require(Quandl)
    require(PerformanceAnalytics)
    source('Functions/Quandl.key.R')
    Quandl.api_key(getQuandlKey())
    sp = Quandl(code = "YAHOO/INDEX_GSPC", type = "xts")
    return(sp)
}

getSp5_QuantmodYahooSPTR = function()
{
  require(xts)
  require(PerformanceAnalytics)
  require(quantmod)
  sp = getSymbols('^SP500TR', auto.assign = F)
  return(sp)
}