compareLevels = function(x, reportDate = NULL,  FUN = "VaR", p = 0.99, width = 126, method = "gaussian")
{
  require(xts)
  if(is.null(reportDate))
    reportDate = end(x)
  
  #FUN = match.fun(FUN)
  
  x=na.omit(x)
  
  m = seq(reportDate, length =2, by = "-1 month")[2]
  q = seq(reportDate, length =2, by = "-1 quarter")[2]
  y = seq(reportDate, length =2, by = "-1 year")[2]
  
  if(reportDate %in% index(x))
    thisDate = getBounds(x[paste("/",reportDate, sep = "")], FUN = FUN, p = p, width = width, method = method)
  else 
      thisDate = NA
  if(m >= start(x))
      lastMonth = getBounds(x[paste("/",m, sep = "")], FUN = FUN, p = p, width = width, method = method)
  else
      lastMonth = NA
  
  if(q  >= start(x))
      lastQuarter = getBounds(x[paste("/",q, sep = "")], FUN = FUN, p = p, width = width, method = method)
  else
      lastQuarter = NA
  
  if(y  >= start(x))
      lastYear = getBounds(x[paste("/",y, sep = "")], FUN = FUN, p = p, width = width, method = method)
  else
      lastYear = NA
  
  
  #result = data.frame(paste(thisDate, "%", sep = ""), paste(100*(thisDate - lastMonth)/abs(lastMonth), "%", sep = ""), paste(100*(thisDate - lastQuarter)/abs(lastQuarter), "%", sep = ""), paste(100*(thisDate - lastYear)/abs(lastYear), "%", sep = ""))
  #result = data.frame(thisDate, 100*(thisDate - lastMonth)/abs(lastMonth), 100*(thisDate - lastQuarter)/abs(lastQuarter), 100*(thisDate - lastYear)/abs(lastYear))
  result = data.frame(thisDate, thisDate - lastMonth, thisDate - lastQuarter, thisDate - lastYear)
  names(result) = c(paste(FUN,as.character.Date(reportDate), sep = " "),"1m change", "1q change", "1y change")
  rownames(result) = paste(FUN, paste(p,"%", sep = ""), sep = " ")
  return(result)
}