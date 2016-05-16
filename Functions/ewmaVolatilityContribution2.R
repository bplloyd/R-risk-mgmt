ewmaVolatilityContribution2 = function(R, weights, lambda = 0.92)
{
  #rtn = na.omit(subs$Coe)
#   require(xts)
#   uft = na.omit(ufts$LSE)['2015/',]
#   allocations.lse = rollingAllocations(785, start = start(uft), end = end(uft))
  R=subs[,names(allocations.lse)]
  weights = lse.alloc
  # rm(V1)
  # R = subs[, names(weights)]
  #R = subs[,names(lse.alloc)]
  weights = na.fill(weights, 0)
  Mu = colMeans(R, na.rm = T)
  miscCol = grep("Misc", names(R))
  if(length(miscCol)>0)
  {
    R = R[which(rowSums(is.na(R[,-miscCol]))<ncol(R[,-miscCol])),]
  }
  if(length(miscCol)==0){
    R = R[which(rowSums(is.na(R)) < ncol(R)),]
  }
  
  if (!is.matrix(R)) {
    rtn_full = as.matrix(R)
  }
  rtn_full = R
  rtn = R[paste0(start(na.omit(R)), "/"),]
  rtn = na.fill(rtn, 0)
  nT = dim(rtn)[1]
  k = dim(rtn)[2]
  
  #x = scale(rtn, center = TRUE, scale = FALSE)
  x_full = scale(rtn_full, center = TRUE, scale = FALSE)
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
  
  if(nrow(rtn)>nrow(weights)){
    firstWeightRow = which(index(rtn)==start(weights))
  }
  if(nrow(rtn)<=nrow(weights)){
    firstWeightRow = 1
    w = as.vector(weights[1,])
    V1 = w*(w %*% Sigt)/rep(sqrt(t(w) %*% Sigt %*% w), ncol(Sigt))
  }

  for (t in 2:nT) {
    xx = as.numeric(rtn[t - 1, ])
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
  # }
  sigma.t = xts(V1, order.by = index(weights))
  names(sigma.t) = colnames(Sigt)
  return(sigma.t)
}


