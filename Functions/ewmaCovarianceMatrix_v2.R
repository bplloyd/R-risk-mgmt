ewmaCovarianceMatrix_v2 = function(rtn, lambda=0.93, na.mode = "zero")
{
  
  if (!is.matrix(rtn)) {
    rtn_full = as.matrix(rtn)
  }
  rtn_full = rtn
  rtn = switch(na.mode, omit = na.omit(rtn), zero = na.fill(rtn, fill = 0))
  nT = dim(rtn)[1]
  k = dim(rtn)[2]
  
  x_full =scale(rtn_full, center = TRUE, scale = FALSE)
  Sigt = cov(x_full, use = "p")
  par = lambda
 
  h1 = 1 - lambda
  
  x = switch(na.mode, omit = na.omit(x_full), zero = na.fill(x_full, fill = 0))
  for (t in 2:nT) {
    xx = as.numeric(x[t - 1, ])
    for (i in 1:k) {
      Sigt[i, ] = h1 * xx * xx[i] + lambda * Sigt[i, 
                                                  ]
    }
  }
  return(Sigt)
}
