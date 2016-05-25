ewmaVolatilityContribution2 = function(R, weights, lambda = 0.92, includeMisc = F)
{
#  rm(V1)
  R=subs[-nrow(subs), names(alpha.allocs.subs)]
  weights = lag(alpha.allocs.subs)
  lambda = 0.92
  miscColR = grep("Misc", names(R))
  miscColW = grep("Misc", names(weights))
  weights = na.fill(weights, 0)
  if(!includeMisc)
  {
    R=R[,-miscColR]
    weights = weights[,-miscColW]
    weights = as.xts(t(apply(weights, 1, FUN = function(x)return(x/sum(x)))))
  }
  index(R) = as.Date(index(R))
  index(weights) = as.Date(index(weights))
  
  R = R[, names(weights)]
 
  Mu = colMeans(R, na.rm = T)
  
  
  if(length(miscCol)>0)
  {
    R = R[which(rowSums(is.na(R[,-miscCol]))<ncol(R[,-miscCol])),]
  }
  if(length(miscCol)==0){
    R = R[which(rowSums(is.na(R)) < ncol(R)),]
  }
  
#   if (!is.matrix(R)) {
#     rtn_full = as.matrix(R)
#   }
  rtn_full = R
  rtn = na.fill(R, 0)
  #rtn = R[paste0(start(na.omit(R)), "/"),]
  
  nT = dim(rtn)[1]
  k = dim(rtn)[2]
  
  #x = scale(rtn, center = TRUE, scale = FALSE)
  x_full = scale(rtn_full, center = TRUE, scale = FALSE)
  Sigt = na.fill(cov(x_full, use = "p"), fill=0)
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
  
  if(nrow(rtn)>nrow(weights)){
    firstWeightRow = which(index(rtn)==start(weights))
  }
  if(nrow(rtn)<=nrow(weights)){
    firstWeightRow = 1
    w = as.vector(weights[1,])
    V1 = w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w), ncol(Sigt))
  }

  for (t in 2:nT) {
    xx = na.fill(as.numeric(rtn[t - 1, ]), fill=0)
    for (i in 1:k) {
      Sigt[i, ] = h1 * xx * xx[i] + lambda * Sigt[i,]
    }
    if(t >= firstWeightRow)
    {
        
        if(!exists("V1"))
        {
            w = as.vector(weights[1,])
            V1 = w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w), ncol(Sigt))
        }
        else
        {
            w = as.vector(weights[t-firstWeightRow + 1, ])
            V1 = rbind(V1, w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w),ncol(Sigt)))
        }
    }
 
  }
  sigma.t = xts(V1, order.by = index(weights))
  names(sigma.t) = colnames(Sigt)
  return(sigma.t*sqrt(252))
}


