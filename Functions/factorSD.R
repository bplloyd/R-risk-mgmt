factor.sd = function(R, weights, betas, residual.sd, newFactorData)
{
out.var = t(weights) %*% (na.fill(betas,0) %*% cov(newFactorData) %*% t(na.fill(betas,0)) + diag(residual.sd))%*% weights
return(sqrt(out.var))
}

