ewmaVolatilityContribution = function(rtn, lambda=0.93)
{
  #rtn = na.omit(subs$Coe)
  require(xts)
  if (!is.matrix(rtn)) {
    rtn_full = as.matrix(rtn)
  }
  rtn_full = rtn
  rtn = na.omit(rtn)
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
  
  x = na.omit(x_full)
  if(k > 1)
  {
      V1 = colSums(Sigt)/sqrt(sum(colSums(Sigt)))
      
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
    if(k > 1)
      V1 = rbind(V1, colSums(Sigt)/sqrt(sum(colSums(Sigt))))
    else
      V1 = rbind(V1, sqrt(Sigt))
  }
  # }
  sigma.t = xts(V1, order.by = index(rtn))
  names(sigma.t) = colnames(Sigt)
  return(sqrt(252)*sigma.t)
}


