rollTsfm2 = function(assets, factors, fit.method = "LS", variable.selection = "none", window = 126, on = "days", includeMisc = F){
  require(factorAnalytics)
  require(fPortfolio)
  require(xts)
  if((!includeMisc) & (length(grep("Misc", names(assets))>0)>0))
      assets = assets[,-grep("Misc", names(assets))]
  
  assets = assets[which(rowSums(is.na(assets)) < ncol(assets)),]
  merged = merge.xts(assets, factors[index(assets),])
  slices = createTimeSlices2(merged, initialWindow = window, fixedWindow = T, on = on)
  
  return(lapply(slices,
                        FUN = function(x)return(fitTsfm(asset.names = names(assets)[which(colSums(is.na(assets[x,]))==0)], 
                                                        factor.names = names(factors),
                                                        data = merged[x,],
                                                        fit.method = fit.method,
                                                        variable.selection = variable.selection
                                                        )
                                                )
                        )
        )
  
#   for(i in 1:length(rollingModel)){
#       from = slices[[i]][1]
#       to = slices[[i]][window]
#       
#       names(rollingModel)[i]= paste(to)
#       rollingModel[[i]] = fitTsfm( 
#                             asset.names = names(assets), factor.names = names(factors),
#                             data = merged[slices[[i]],],
#                             fit.method = fit.method,
#                             variable.selection = variable.selection
#                           )
#   }
  #return(rollingModel)
}