getCBOE = function()
{
  require(Quandl)
  require(xts)
  source("Functions/Quandl.key.R")
  Quandl.api_key(getQuandlKey())
  
  codes = c("VIX", "VXV", "TYVIX", "SRVX", "VVIX", "SKEW", "SPX_PC" ,"VIX_PC")
  codes = paste("CBOE", codes, sep = "/")
  
  cboe = Quandl(code = codes, type = "xts", meta = F)
  keepCols = sort(
                  c(
                      grep("CLOSE", toupper(names(cboe))), 
                      grep("SKEW", toupper(names(cboe))),
                      grep("RATIO", toupper(names(cboe))),
                      grep("VVIX", toupper(names(cboe)))
                    )
  )
  return(cboe[,keepCols])
}