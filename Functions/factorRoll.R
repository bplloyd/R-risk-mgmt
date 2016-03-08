factorRoll = function(assets, factors, fit.method = "LS", variable.selection = "none", window = 126, on = "days", includeMisc = F)
{
    rollFit = rollTsfm2(assets, factors, fit.method = fit.method, variable.selection = variable.selection, window =  window, on = on, includeMisc = includeMisc)
    rollFit.coef = lapply(rollFit, function(x)return(coef(x)))
    
    factorRoll = array(data = NA, dim = c(length(rollFit.coef), 10, 5), dimnames = c(names(rollFit.coef), col.names(rollFit.coef[[length(rollFit.coef)]]), row.names(lse.factors.coef[[length(rollFit.coef)]])))
}

  