rollingAlpha = function(accounts, bm, width = 126, scale = NULL)
{
  require(PerformanceAnalytics)
  require(xts)
  if(is.null(scale))
      scale = Frequency(accounts)
  
  if(ncol(accounts)==1)
  {
      merged = na.omit(merge.xts(accounts, bm))
      return(rollapply(merged,width = width, FUN = function(x)return(lm(x[, 1,drop = FALSE] ~ x[, 2, drop = FALSE])$coefficients[1]), by = 1, by.column = FALSE, align = "right"))
  }
  else
  {
      temp = accounts[,1]
      temp.bm = bm
      result = rollingAlpha(temp, temp.bm, width = width, scale = scale)
      for(i in 2:ncol(accounts))
      {
        temp = rollingAlpha(xts(accounts[,i], order.by = index(accounts)), temp.bm, width = width, scale = scale)
        result = merge.xts(result, temp)
      }
      names(result) = names(accounts)
      return(result)
  }
}