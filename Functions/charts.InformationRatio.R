charts.InformationRatio = function(R, bm, width = 63) 
{
  
  data = rollingInformationRatio(Ra = R, Rb = bm, width = width)
  layout(matrix(c(1, 2,3)), heights = c(1,1,1), widths = 1)
  par(mar = c(3, 3, 3, 3))
  chart.TimeSeries(data$ActivePremium)
  par(mar = c(3, 3, 3, 3))
  chart.TimeSeries(data$TrackingError)
  par(mar = c(3, 3, 3, 3))
  chart.TimeSeries(data$InformationRatio)
}