sharpeDecomp = function(port, weights){
  library(PerformanceAnalytics);
  port_ret = Return.portfolio(port, weights = weights);
  port.sd = StdDev.annualized(port_ret)[1,1];
  sd_assets = StdDev.annualized(port, portfolio_method = "single", weights = rep(1/ncol(port), ncol(port)), use = "complete")[1,];
  sharpe_assets = SharpeRatio.annualized(port)[1,];
  cor_toPort = table.Correlation(port, port_ret)[,1];
  Weighted_risk = sd_assets*cor_toPort*weights/port.sd;
  comp_sharpe = sharpe_assets * (1/cor_toPort);
  ret = rbind(Weighted_risk, cor_toPort, 1/cor_toPort, sharpe_assets, comp_sharpe, Weighted_risk*comp_sharpe);
  rownames(ret) = c('CondContribToRisk', 'CorrToPort', 'DiversBenefits', 'IndivSharpe', 'CompSharpe', 'ContribuToSharpe');
  return(ret);
}
