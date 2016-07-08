TrackingError_edit = function (Ra, Rb, scale = NA) 
{
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  Ra.ncols = NCOL(Ra)
  Rb.ncols = NCOL(Rb)
  pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
  if (is.na(scale)) {
    freq = periodicity(Ra)
    switch(freq$scale, minute = {
      stop("Data periodicity too high")
    }, hourly = {
      stop("Data periodicity too high")
    }, daily = {
      scale = 252
    }, weekly = {
      scale = 52
    }, monthly = {
      scale = 12
    }, quarterly = {
      scale = 4
    }, yearly = {
      scale = 1
    })
  }
  terr <- function(Ra, Rb, scale) {
    TE = sd(Return.excess_edit(Ra, Rb), na.rm = TRUE) * sqrt(scale)
    return(TE)
  }
  result = apply(pairs, 1, FUN = function(n, Ra, Rb, scale) terr(Ra[, 
                                                                  n[1]], Rb[, n[2]], scale), Ra = Ra, Rb = Rb, scale = scale)
  if (length(result) == 1) 
    return(result)
  else {
    dim(result) = c(Ra.ncols, Rb.ncols)
    colnames(result) = paste("Tracking Error:", colnames(Rb))
    rownames(result) = colnames(Ra)
    return(t(result))
  }
}