toXts = function(x, name)
{
  res = xts(x[,2], order.by = as.Date.character(x$Date, format = '%m/%d/%Y'))
  names(res) = c(name)
  return(res)
}