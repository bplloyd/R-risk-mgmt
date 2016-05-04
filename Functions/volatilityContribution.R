volatilityContribution = function(tbl)
{
  sig = cov(x = tbl, y = tbl)
  #out = as.data.frame(t(colSums(sig)/sd(apply(tbl,1,sum))))
  out = colSums(sig)/sqrt(sum(colSums(sig)))
  #row.names(out) = "ContributionToVolatility"
  return(out)
}