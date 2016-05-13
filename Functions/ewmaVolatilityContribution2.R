ewmaVolatilityContribution2 = function(R, weights, lambda = 0.92)
{
  #rtn = na.omit(subs$Coe)
  require(xts)
  R = R[which(rowSums(is.na(R))<ncol(R)),]
  Mu = colMeans(R, na.rm = T)
  
  if (!is.matrix(R)) {
    rtn_full = as.matrix(R)
  }
  rtn_full = R
  rtn = R[paste0(start(na.omit(R)), "/"),]
  rtn = na.fill(rtn, 0)
  nT = dim(rtn)[1]
  k = dim(rtn)[2]
  
  #x = scale(rtn, center = TRUE, scale = FALSE)
  x_full =scale(rtn_full, center = TRUE, scale = FALSE)
  Sigt = cov(x_full, use = "p")
  par = lambda
  #   MGAUS <- function(par, x = x) {
  #     lambda = par[1]
  #     h1 = 1 - lambda
  #     Sigt = cov(x)
  #     lk = 0
  #     nT = dim(x)[1]
  #     k = dim(x)[2]
  #     for (t in 2:nT) {
  #       xx = as.numeric(x[t - 1, ])
  #       for (i in 1:k) {
  #         Sigt[i, ] = h1 * xx[i] * xx + lambda * Sigt[i, 
  #                                                     ]
  #       }
  #       ll = dmvnorm(x[t, ], rep(0, k), sigma = Sigt, log = TRUE)
  #       lk = lk - ll
  #     }
  #     lk
  #   }
  # if (lambda > 0) {
  h1 = 1 - lambda
  
  if(is.matrix(weights))
  {
      if(nrow(R)>nrow(weights)){
          firstWeightRow = which(index(rtn)==start(weights))
      }
      else{
          firstWeightRow = 1
          V1 = weights_t*(weights[1,]%*%Sigt)/rep(sqrt(t(weights[1,])%*%Sigt%*%weights[1,]), 3)
      }
  }
  
  if(is.vector(weights))
    weights_t = weights
  if(is.matrix(weights))
    weights_t = weights[1,]
  
  
  
  if((k > 1) & (nrow(weights) >= nrow(rtn)))
  {
      V1 = weights_t*(weights_t%*%Sigt)/rep(sqrt(t(weights_t)%*%Sigt%*%weights_t), 3)
      
  }
  if(k==1)
  {
      V1 = sqrt(Sigt)
  }
  
  
  for (t in 2:nT) {
    xx = as.numeric(x[t - 1, ])
    for (i in 1:k) {
      Sigt[i, ] = h1 * xx * xx[i] + lambda * Sigt[i, 
                                                  ]
    }
    if((k > 1) & (nrow(weights) >= nrow(rtn)))
    {
      V1 = rbind(V1, weights_t*(weights_t %*% Sigt)/rep(sqrt(t(weights_t) %*% Sigt %*% weights_t), 3))
    else
      V1 = rbind(V1, sqrt(Sigt))
  }
  # }
  sigma.t = xts(V1, order.by = index(rtn))
  names(sigma.t) = colnames(Sigt)
  return(sigma.t)
}


