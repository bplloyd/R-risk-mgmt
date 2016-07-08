minVarPort = function(cov.mat)
{
  cov.mat.inv = solve(cov.mat)
  minVarP = rowSums(cov.mat.inv)/sum(cov.mat.inv)
  return(minVarP)
}