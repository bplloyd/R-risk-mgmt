# RETURNS VaR OR ES BOUNDS USING FUNCTIONS FROM PERFORMANCE ANALYTICS PACKAGE

getBounds = function(x, FUN = "VaR", p = 0.99, width = 126, method = "modified"){
  FUN = match.fun(FUN)
  result = do.call(FUN, list(R = x, p=p, width =width, method = method, portfolio_method = "single"))
  return(result)
}

