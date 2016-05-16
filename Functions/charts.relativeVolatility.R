charts.relativeVolatility = function(Ra, Rb, width = 63)
{
  Ra = na.omit(Ra)
  Rb = na.omit(Rb)
  rollingData = rollingAlphaBeta(Ra, Rb, width = width)
  index(rollingData) = as.Date(index(rollingData))
  correl = sqrt(rollingData[,3])
  relVol = rollapply(Ra, width = width, FUN = function(x)return(sd(x, na.rm = T)))/rollapply(Rb, width = width, FUN = function(x)return(sd(x, na.rm = T)))
  
  beta = rollingData[,2]
  
  chartData = cbind(relVol, beta, correl)
  names(chartData) = c("Beta", "Correlation", "RelativeVolatility")
  chart.TimeSeries(chartData, legend.loc = "topleft", main = paste0("Relative Volatility - ", names(Ra), " vs ", names(Rb)))
  
}