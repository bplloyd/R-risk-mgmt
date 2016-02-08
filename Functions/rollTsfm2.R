rollTsfm2 = function(assets, factors, data, fit.method = "LS", variable.selection = "none", slices){
  require(factorAnalytics)
  require(fPortfolio)
  require(xts)

  rollingModel = vector(mode = "list", length = length(slices[[1]]))
  for(i in 1:length(rollingModel)){
      from = index(data)[slices[[1]][[i]][1]]
      to = index(data)[slices[[1]][[i]][length(slices[[1]][[i]])]]
      names(rollingModel)[i]= paste(to)
      rollingModel[[i]] = fitTsfm( 
                            asset.names = assets, factor.names = factors,
                            data = data[slices[[1]][[i]],],
                            fit.method = fit.method,
                            variable.selection = variable.selection
                          )
  }
  return(rollingModel)
}