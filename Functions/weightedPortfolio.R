weightedPortfolio = function(R, weights)
{
  R = na.omit(R[, names(weights)])
  res = xts(apply(R, MARGIN = 1, FUN = function(r)return(sum(r*weights))), order.by = index(R))
  index(res) = as.Date(index(res))
  return(res)
}
