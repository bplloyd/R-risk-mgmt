ewmaCovarianceMatrix_Full = function(rtn, lambda=0.94)
{
  #require(xts)
  #require(stringr)
  
  if (!is.matrix(rtn)) {
    rtn_full = as.matrix(rtn)
  }
  #startDates = as.Date.numeric(sapply(names(rtn), function(n)return(start(na.omit(rtn[,n])))))
  rtn_full = rtn
  #rtn = na.omit(rtn)
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
  
  #x = na.omit(x_full)
  x=x_full
  
  #V1 = c(Sigt)
  for (t in 2:nT) {
    xx = as.numeric(x[t - 1, ])
    for (i in 1:k) {
      for(j in 1:k){
        if((!is.na(xx[j])) & (!is.na(xx[i])))
          Sigt[i, j] = h1 * xx[j] * xx[i] + lambda * Sigt[i, j]
      }                                         
    }
    #V1 = rbind(V1, c(Sigt))
  }
  # }
  #sigma.t = xts(V1, order.by = index(rtn))
  #cols = vector(mode = "character", length = k^2)
#   for(i in 1:k){
#     cols[((i-1)*k+1):(i*k)] = paste(names(rtn)[i], names(rtn), sep = "_")
#   }
#   names(sigma.t) = cols
#   splitNames = str_split(names(sigma.t), "_")
#   for(i in 1:ncol(sigma.t))
#   {
#     pairStart = max(startDates[splitNames[[i]][1]], startDates[splitNames[[i]][2]])
#     sigma.t[index(sigma.t) < pairStart,i] = NA
#   }
#   return(sigma.t)
  return(Sigt)
}
