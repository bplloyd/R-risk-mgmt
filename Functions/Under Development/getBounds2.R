# RETURNS VaR OR ES BOUNDS USING FUNCTIONS FROM PERFORMANCE ANALYTICS PACKAGE

getBounds2 = function(x, FUN = "VaR", ...){
  FUN = match.fun(FUN)
  result = do.call(FUN, list(x=x, list(...)))
  return(result)
}

