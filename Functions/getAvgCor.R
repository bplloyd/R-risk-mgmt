getAvgCor = function(portfolio, benchmark, n = 126, use = "pairwise.complete.obs", reportDate = NULL, includeMisc = F)
{
    require(xts)
    if (is.null(reportDate))
        reportDate = end(portfolio)
    if((!includeMisc) & (length(grep('Misc', names(portfolio))) > 0)){
        portfolio = portfolio[,-grep('Misc', names(portfolio))]
    }
    
    portfolio = portfolio[paste0("/", reportDate)]
    benchmark = benchmark[paste0("/", reportDate)]
    return(mean(cor(portfolio[(nrow(portfolio)-n+1):nrow(portfolio),], benchmark[(nrow(benchmark)-n+1):nrow(benchmark)], use = "p")))
}