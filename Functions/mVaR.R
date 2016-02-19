mVaR = function(x, p = 0.99, n = NULL){
  require(PerformanceAnalytics)
  x = na.omit(x)
  if (!is.null(n)){
      nr = nrow(x)
      x = x[(nr - n + 1):nr]
  }
  z = qnorm(1-p)
  s = skewness(x)
  k = kurtosis(x)
  return(mean(x) + sd(x)*(z + (s/6)*(z^2 - 1) + (k/24)*(z^3 - 3*z) - ((s^2)/36)*(2*z^3 - 5*z)))
}