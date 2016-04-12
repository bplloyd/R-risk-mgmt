charts.InformationRatio = function(R, bm, width = 126) 
{

  data = cbind(rollCompare(R, bm, width = width, FUNC = "ActivePremium"), rollCompare(R, bm, width = width, FUNC = "TrackingError"))
  names(data) = c("ActivePremium", "TrackingError")
  data$InformationRatio = data$ActivePremium/data$TrackingError
  layout(matrix(c(1, 2,3)), heights = c(1,1,1), widths = 1)
  par(mar = c(3, 3, 3, 3))
  chart.TimeSeries(data$ActivePremium)
  par(mar = c(3, 3, 3, 3))
  chart.TimeSeries(data$TrackingError)
  par(mar = c(3, 3, 3, 3))
  chart.TimeSeries(data$InformationRatio)
}