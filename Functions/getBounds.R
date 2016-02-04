getBounds = function(x, FUN = FUN, p = 0.99, width = 126, method = "modified"){
  if(FUN == "VaR"){
    result = merge.xts(apply.rolling2(x, FUN = FUN, width = width, p = p, method = method, portfolio_method="single"), apply.rolling2(x,  FUN = FUN, width = width,p = 1-p, method = method, portfolio_method="single"))
    names(result)[1:2] = c(paste(FUN, p, sep = "_"), paste(FUN, 1-p, sep = "_"))
  }
  if(FUN == "ES"){
    result =apply.rolling2(x, FUN = FUN, width = width, p = p, method = method, portfolio_method="single")
    names(result)[1] = paste(FUN, p, sep = "_")
  }
  return(result)
}