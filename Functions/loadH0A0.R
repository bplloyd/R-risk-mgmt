loadH0A0 = function()
{ require(Quandl)
  source("Functions/Quandl.key.R")
  require(xts)
  Quandl.api_key(quandl.key)
  codes = c("HYOAS", "USTRI")
  codes = paste0("ML/", codes)
  return(Quandl(code = codes, type = "xts"))
}