mVaR = function(x, p = 0.98, n = NULL){
  require(PerformanceAnalytics)
  x = na.omit(x)
  if(!(is.null(n)))
  {
      if(n > nrow(x))
      {
          return(NA)
      }
      else
      {
          nr = nrow(x)
          x = x[(nr - n + 1):nr]
      }
  }
  z = qnorm(1-p)
  s = skewness(x, method = "sample")
  k = kurtosis(x, method = "sample_excess")
  return(mean(x) + sd(x)*(z + (s/6)*(z^2 - 1) + (k/24)*(z^3 - 3*z) - ((s^2)/36)*(2*z^3 - 5*z)))
}