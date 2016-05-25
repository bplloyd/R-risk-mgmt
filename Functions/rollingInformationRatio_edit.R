rollingInformationRatio_edit = function(Ra, Rb, width = 63, on = "days")
{
  require(PerformanceAnalytics)
  require(xts)
  
  data = na.omit(cbind(Ra, Rb))
  Ra = data[, names(Ra)]
  Rb = data[, names(Rb)]
  slices = createTimeSlices2(data, initialWindow = width, on = on)
  
  res = as.xts(t(sapply(slices, FUN = function(t)
    return(cbind(ActivePremium(data[t,1],data[t,2]), 
                 TrackingError_edit(data[t,1],data[t,2])
    )
    )
  )
  )
  )
  names(res) = c("ActivePremium", "TrackingError")
  res$InformationRatio = res$ActivePremium/res$TrackingError
  
  
  names(res) = paste(names(res), names(Rb)[1], sep = "_")
  
  if(ncol(Rb)>1){
    for(i in 2:ncol(Rb)){
      data = na.omit(cbind(Ra, Rb[,i]))
      slices = createTimeSlices2(data, initialWindow = width, on = on)
      res = cbind(res,as.xts(t(sapply(slices, FUN = function(t)
        return(cbind(ActivePremium(data[t,1],data[t,2]), 
                     TrackingError_edit(data[t,1],data[t,2])
        )
        )
      )
      )
      )
      )
      names(res)[(3*(i-1)+1):(3*(i-1)+2)] = c("ActivePremium", "TrackingError")
      res$InformationRatio = res$ActivePremium/res$TrackingError
      names(res)[(3*(i-1)+1):(3*i)] = paste(names(res)[(3*(i-1)+1):(3*i)], names(Rb)[i], sep = "_")
    }
  }
  
  return(res)
}



rollingInformationRatio2 = function(Ra, Rb, width = 63, on = "days")
{
  require(PerformanceAnalytics)
  require(xts)
  data = na.omit(cbind(Ra, Rb))
  Ra = data[, names(Ra)]
  Rb = data[, names(Rb)]
  slices = createTimeSlices2(data, initialWindow = width, on = on)
  res = as.xts(t(sapply(slices, FUN = function(t)InformationRatio(Ra[t,],Rb[t,]))))
  return(res)
}