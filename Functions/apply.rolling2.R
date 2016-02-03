apply.rolling2 = function (R, width, trim = TRUE, gap = 12, by = 1, FUN = "mean", 
                           ...) 
{
  #R = checkData(R)
  #R = na.omit(R)
  rows = NROW(R)
  cols = NCOL(R)
  result = xts(, order.by = time(R))
  dates = time(R)
  calcs = matrix(ncol = cols)
  if (width == 0) {
    gap = gap
  }
  else gap = width
  steps = seq(from = rows, to = gap, by = -by)
  steps = steps[order(steps)]
  for (row in steps) {
    if (width == 0) 
      r = R[1:row, ]
    else r = R[(row - width + 1):row, ]
    calc = apply(r, MARGIN = 2, FUN = FUN, ... = ...)
    calcs = rbind(calcs, calc)
  }
  calcs = xts(calcs[-1,], order.by = dates[steps])
  result = merge(result, calcs)
  result = reclass(result, R)
  return(result)
}