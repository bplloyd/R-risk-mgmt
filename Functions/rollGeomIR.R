rollGeomIR = function(Ra, Rb, width = 63, beta = NULL)
{
  require(xts)
  require(PerformanceAnalytics)
  tslices = createTimeSlices2(data = na.omit(cbind(Ra, Rb)), initialWindow = width)
  res = as.xts(t(sapply(tslices, FUN = function(t)return(geomIR(Ra[t], Rb[t], beta)))))
  names(res) = c("GeomActivePremium", "GeomTrackingError", "GeomInformationRatio")
  index(res) = as.Date(index(res))
  return(res)
}