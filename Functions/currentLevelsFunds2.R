currentLevelsFunds2 = function(funds, FUN = "VaR", p=0.98, width = 126, method = "modified", model = "historical", weights = NULL, subs.o=NULL, mode = "percent")
{
  if(model == "weights")
  {
      if(is.null(subs.o))
        subs.o = organizeSubs()
      if(is.null(weights))
          weights = getWeights(subs.o)
      funds = weightedPortfolios(subs.o = subs.o, subs.weights = subs.weights)
  }
  
  levels = apply(funds, 2, function(x)compareLevels2(as.xts(x), FUN = FUN, p = p, width = width, method = method, mode = mode))
  result = as.data.frame(matrix(unlist(levels), nrow = length(levels), byrow = T, dimnames = list(names(levels), colnames(levels[[1]]))))
  return(result)
}