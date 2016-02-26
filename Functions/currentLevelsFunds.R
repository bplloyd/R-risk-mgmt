currentLevelsFunds = function(funds, FUN = "VaR", p=0.99, width = 126, method = "modified")
{
  levels = apply(funds, 2, function(x)compareLevels(as.xts(x), FUN = FUN, p = p, width = width, method = method))
  result = as.data.frame(matrix(unlist(levels), nrow = length(levels), byrow = T, dimnames = list(names(levels), colnames(levels[[1]]))))
  return(result)
}