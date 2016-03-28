factorRoll = function(assets, factors, fit.method = "LS", variable.selection = "none", window = 126, on = "days", includeMisc = F)
{
    rollFit = rollTsfm2(assets, factors, fit.method = fit.method, variable.selection = variable.selection, window =  window, on = on, includeMisc = includeMisc)
    rollFit.coef = lapply(rollFit, function(x)return(coef(x)))
    
    factorRoll = array(data = NA, dim = c(length(rollFit), ncol(factors)+1, ncol(assets)), dimnames = list(names(rollFit), c("intercept",names(factors)), names(assets)))
    for(i in 1:ncol(assets)){
      factorRoll[,,i] = as.matrix(na.fill(extractSingleBetaroll(rollFit, names(assets)[i]), fill = 0))
    }
    return(factorRoll)
    
}

  