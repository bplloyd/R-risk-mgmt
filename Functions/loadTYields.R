loadTYields = function()
{
  library(Quandl)
  Quandl.api_key(getQuandlKey())
  return(Quandl(code="USTREASURY/YIELD", type = "xts"))
}

loadZeroCurve = function()
{
  library(Quandl)
  Quandl.api_key(getQuandlKey())
  return(Quandl(code="FED/SVENY", type = "xts"))
}

hyOAS = function()
{
  library(Quandl)
  Quandl.api_key(getQuandlKey())
  oas = Quandl(code="ML/HYOAS", type = "xts")
  names(oas)[1] = "HYOAS"
  return(oas)
}

fiFactors = function()
{
  library(Quandl)
  Quandl.api_key(getQuandlKey())
  return(Quandl(code = c("ML/HYOAS","USTREASURY/YIELD"), type = "xts"))
}