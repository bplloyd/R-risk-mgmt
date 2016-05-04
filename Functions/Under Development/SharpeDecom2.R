sharpeDecomp = function(port, weights=NULL){
  require(PerformanceAnalytics)
  #require(magrittr)
  if(is.null(weights)){
      weights = rep(1/ncol(port), ncol(port))
  }else{
      weights = weights/sum(weights)
  }
  port.ret = Return.portfolio(port, weights = weights, geometric = F);
  port.sd = StdDev.annualized(port.ret)[1,1];
  ind.sd = StdDev.annualized(port, portfolio_method = "single")[1,];
  ind.sharpe = SharpeRatio.annualized(port)[1,];
  ind.corToPort = table.Correlation(port, port.ret)[,1];
  ind.riskWeight = ind.sd*ind.corToPort*weights/port.sd;
  componentSharpe = ind.sharpe * (1/ind.corToPort);
  result = rbind(weights, ind.riskWeight, ind.corToPort, 1/ind.corToPort, ind.sharpe, componentSharpe, ind.riskWeight*componentSharpe);
  rownames(result) = c('Weights','WeightedRisk', 'CorrToPort', 'DiversBenefits', 'IndivSharpe', 'CompSharpe', 'ContributionToSharpe');
  return(result);
}
sharpeDecomp2 = function(port, weights=NULL, Rf = 0){
  require(PerformanceAnalytics)
  require(magrittr)
  if(is.null(weights)){
    weights = rep(1/ncol(port), ncol(port))
  }else{
    weights = weights/sum(weights)
  }
  port.ret = port %>% apply(1, function(x)return(sum(x*weights))) %>% xts(order.by = index(port))
  port.sd = (port.ret %>% sd())*sqrt(Frequency(port)[1])
  ind.ret = (port %>% apply(MARGIN = 2, FUN = function(x)return((1+x) %>% log() %>% mean() %>% exp())))^(Frequency(port)[1]) - 1
  ind.sd = (port %>% apply( MARGIN = 2, FUN = sd))*sqrt(Frequency(port)[1])
  ind.sharpe = (ind.ret - Rf)/ind.sd
  ind.corToPort = table.Correlation(port, port.ret)[,1]
  ind.riskWeight = ind.sd*ind.corToPort*weights/port.sd
  componentSharpe = ind.sharpe * (1/ind.corToPort)
  result = rbind(weights, ind.riskWeight, ind.corToPort, 1/ind.corToPort, ind.sharpe, componentSharpe, ind.riskWeight*componentSharpe)
  rownames(result) = c('Weights','WeightedRisk', 'CorrToPort', 'DiversBenefits', 'IndivSharpe', 'CompSharpe', 'ContributionToSharpe')
  return(result)
}

  
sharpeDecomp3 = function(port, weights=NULL, Rf = 0){
  require(PerformanceAnalytics)
  require(magrittr)
  if(is.null(weights)){
    weights = rep(1/ncol(port), ncol(port))
  }else{
    weights = weights/sum(weights)
  }
  freq = Frequency(port)[1]
  port.ret = port %>% apply(1, function(x)return(x%*%weights)) %>% xts(order.by = index(port))
  port.sd = (port.ret %>% sd())*sqrt(freq)
  ind.ret = (port %>% apply( MARGIN = 2, FUN = function(x)return(exp(sum(log(1+x)))-1)))^freq
  ind.sd = (port %>% apply( MARGIN = 2, FUN = sd))*sqrt(freq)
  ind.sharpe = port %>% apply(MARGIN = 2, FUN = function(x)return(mean))
  ind.corToPort = table.Correlation(port, port.ret)[,1];
  ind.riskWeight = ind.sd*ind.corToPort*weights/port.sd;
  componentSharpe = ind.sharpe * (1/ind.corToPort);
  result = rbind(weights, ind.riskWeight, ind.corToPort, 1/ind.corToPort, ind.sharpe, componentSharpe, ind.riskWeight*componentSharpe);
  rownames(result) = c('Weights','WeightedRisk', 'CorrToPort', 'DiversBenefits', 'IndivSharpe', 'CompSharpe', 'ContributionToSharpe');
  return(result)
}