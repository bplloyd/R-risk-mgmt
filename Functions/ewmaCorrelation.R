ewmaCorrelation = function(ewmaCov)
{
  require(stringr)
  require(xts)
  k = sqrt(ncol(ewmaCov))
  n = factorial(k)/(factorial(2)*(factorial(k-2)))
  M = matrix(names(ewmaCov), ncol = k)
  D = diag(M)
  L = M[lower.tri(M)]
  
  nms = str_sub(D, str_locate(D, "_")[,1]+1)
  vars = ewmaCov[,D]
  covars = ewmaCov[,L]
  names(vars) = nms
  result = matrix(, nrow = nrow(ewmaCov), ncol = length(names(covars)))
  colnames(result) = names(covars)
  for(i in 1:length(names(covars)))
  {
    n1 = str_sub(names(covars)[i], end=str_locate(names(covars)[i], "_")[,1]-1)
    n2 = str_sub(names(covars)[i], str_locate(names(covars)[i], "_")[,1]+1)
    result[,i] = covars[,i]/sqrt(vars[,n1]*vars[,n2])
  }
  return(xts(result, order.by = index(ewmaCov)))
}