# RETURNS VaR OR ES BOUNDS USING FUNCTIONS FROM PERFORMANCE ANALYTICS PACKAGE

getBounds = function(x, FUN = "VaR", p = 0.99, width = 126, method = "modified"){
  FUN = match.fun(FUN)
  if(nrow(x)>=width)
      result = do.call(FUN, list(R = x[(nrow(x)-(width -1)):nrow(x)], p=p, method = method, portfolio_method = "single"))
  else
      result = NA
  return(result)
}

