minVaRPortfolio = function(R, method = "Sample", width = NULL, lambda = NULL)
{
  if(!is.null(width))
    R = R[(nrow(R)-width + 1):nrow(R), ]
  if(is.null(lambda))
    lambda = 0.94
  
  cov.mat = switch(toupper(method), 
                   "SAMPLE" = cov(R, use = "p"),
                   "EWMA" = ewmaCovarianceMatrix(R))
  cov.mat.inv = solve(cov.mat)
  return(rowSums(cov.mat.inv)/sum(cov.mat.inv))
}