rollVaR  = function(assets, weights = NULL, p = 0.99, period = "12m", by = "1m", portfolio_method = "component"){
  require(xts)
  require(fPortfolio)
  require(PortfolioAnalytics)
  require(magrittr)
  if(portfolio_method == "component"){
      assets = na.omit(assets)
  }
  if(is.null(weights)){
      weights = rep(1/ncol(assets), ncol(assets))
  }
  else{
      weights = weights/sum(weights)
  }
  wins = rollingWindows(assets, period = period, by = by)
  myList = vector("list",length(wins$from))
  for (i in 1:length(myList)){
    names(myList)[i] = paste0("periodEnding_",wins$to[i])
  }
  
  for(i in 1:length(wins$from)){
    if(portfolio_method == "component"){ 
      myList[[i]] = VaR(R = assets[paste(wins$from[i],"/", wins$to[i], sep = "")], method = "gaussian", p = p, portfolio_method = portfolio_method, weights = weights)
    }
    if(portfolio_method == "single"){
      if(ncol(assets[paste(wins$from[i],"/", wins$to[i], sep = ""),colSums(is.na(assets[paste(wins$from[i],"/", wins$to[i], sep = "")]))==0])>0)
        myList[[i]] = VaR(R = assets[paste(wins$from[i],"/", wins$to[i], sep = ""),colSums(is.na(assets[paste(wins$from[i],"/", wins$to[i], sep = "")]))==0], method = "gaussian", p = p, portfolio_method = portfolio_method)
    }
  }
  
  if(portfolio_method == "component"){
      result <- myList[[1]]$contribution %>% t() %>% as.data.frame()
  }
  if(portfolio_method == "single"){
      result <- myList[[1]] %>% as.data.frame()
  }
  
  for(i in 2:length(wins$from)){
    if(portfolio_method == "component"){
        result = result %>% rbind(myList[[i]]$contribution %>% t() %>% as.data.frame())
    }
    if(portfolio_method == "single"){
      result = result %>% rbind(myList[[i]] %>% as.data.frame())
    }
  }
  if(portfolio_method == "component"){
      result$Total = apply(result, 1, sum)
  }
  return(xts(result, order.by = as.Date.timeDate(wins$to)))
}

