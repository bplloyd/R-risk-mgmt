compareLevels = function(x, reportDate = NULL,  FUN = "VaR", p = 0.98, width = 126, method = "modified")
{
  require(xts)
  require(PerformanceAnalytics)
  if(is.null(reportDate))
    reportDate = end(x)
  
  if(FUN == "VaR")
  {
    if(method == "modified")
        func = function(y){return(mVaR(y, p=p, n=width))}
    else
        func = function(y){return(nVaR(y, p=p, n=width))}
  }
  if(FUN == "ES")
  {
      func = function(y){
        if(nrow(y) < width)
            return(NA)
        else
            return(ES(R = y[(nrow(y)-width+1):nrow(y),], p=p, portfolio_method = "single", method = method))
        }
  }
    
  x=na.omit(x)
  
  m = seq(reportDate, length =2, by = "-1 month")[2]
  q = seq(reportDate, length =2, by = "-1 quarter")[2]
  y = seq(reportDate, length =2, by = "-1 year")[2]
  
#   if((reportDate %in% index(x)) & (nrow(x) >= width))
       thisDate = func(x[paste("/",reportDate, sep = ""),])
#   else 
#       thisDate = NA
#   
#   if(m >= start(x))
       lastMonth = func(x[paste("/",m, sep = ""),])
#   else
#       lastMonth = NA
#   
#   if(q  >= start(x))
      lastQuarter = func(x[paste("/",q, sep = ""),])
#   else
#     lastQuarter = NA
#   
#   if(y  >= start(x))
       lastYear = func(x[paste("/",y, sep = ""),])
#   else
#       lastYear = NA
  
  
  #result = data.frame(paste(thisDate, "%", sep = ""), paste(100*(thisDate - lastMonth)/abs(lastMonth), "%", sep = ""), paste(100*(thisDate - lastQuarter)/abs(lastQuarter), "%", sep = ""), paste(100*(thisDate - lastYear)/abs(lastYear), "%", sep = ""))
  #result = data.frame(thisDate, 100*(thisDate - lastMonth)/abs(lastMonth), 100*(thisDate - lastQuarter)/abs(lastQuarter), 100*(thisDate - lastYear)/abs(lastYear))
  result = data.frame(thisDate, thisDate - lastMonth, thisDate - lastQuarter, thisDate - lastYear)
  names(result) = c(paste(FUN,as.character.Date(reportDate), sep = " "),"M/O/M/ change", "Q/O/Q change", "Y/O/Y change")
  rownames(result) = paste(FUN, paste(p,"%", sep = ""), sep = " ")
  return(result)
}