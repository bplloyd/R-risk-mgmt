readFamaFile = function(filename)
{
  require(lubridate)
  require(xts)
  f <- read.csv(paste0("G://PORTFOLIO MANGEMENT//Bryan Lloyd//2016 Projects//Data//FamaFrench//", filename, ".CSV"), stringsAsFactors=FALSE)
  f$Date = ymd(paste0(f$X[1],"31")) %m+% months(0:(nrow(f)-1))
  f.xts = xts(f[,2:(ncol(f)-1)], order.by = f$Date)
  return(f.xts)
}