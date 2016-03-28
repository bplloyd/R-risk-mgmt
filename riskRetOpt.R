riskRetOpt = function(R, return.func = "mean", return.arguments = list(), risk.func = "ETL", risk.arguments = list(p=0.98), optimize_method = "random", search_size = 30000, min_sum = 0.98, max_sum = 1.02)
{
    require(PortfolioAnalytics)
    require(DEoptim)
    require(ROI)
    require(ROI.plugin.glpk)
    require(ROI.plugin.quadprog)
    
    fund.names = names(R)

    pspec = portfolio.spec(assets = fund.names)
    pspec = add.constraint(portfolio = pspec, type = "weight_sum", min_sum = min_sum, max_sum = max_sum)
    pspec = add.constraint(portfolio = pspec, type = "long_only") 

    minRisk = add.objective(portfolio = pspec, type = 'risk', name = risk.func, arguments = risk.arguments )
    maxRiskRet = add.objective(portfolio = minRisk, type = "return", name = return.func, arguments = return.arguments)

    return(optimize.portfolio(R=R, portfolio = maxRiskRet, optimize_method = optimize_method, trace = T, search_size = search_size))
}


