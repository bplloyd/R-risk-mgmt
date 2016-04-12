reportComponentRisks = function(subs.o, subs.weights, n = 126, method = "gaussian")
{
  comp.sd = organizeComponentRisks(getComponentRisk(subs.o, subs.weights, FUN = "StdDev", n = n))
  comp.sd.p = lapply(comp.sd, function(x){result = data.frame(x[3,]*100); colnames(result)[1] = "StdDev"; return(result)})
  total.sd = lapply(comp.sd, FUN = function(x){temp = data.frame(100*x[1,1]);   colnames(temp)[1] = row.names(x)[1]; rownames(temp)[1] = "Total"; return(temp)})
  
  comp.VaR = organizeComponentRisks(getComponentRisk(subs.o, subs.weights, FUN = "VaR", n = n, method = method))
  colName = switch (method,
    gaussian = "VaR",
    modified = "MVaR"
  )
  comp.VaR.p = lapply(comp.VaR, function(x){result = data.frame(x[3,]*100); colnames(result)[1] = colName; return(result)})
  total.VaR = lapply(comp.VaR, FUN = function(x){temp = data.frame(-100*x[1,1]); colnames(temp)[1] = row.names(x)[1]; rownames(temp)[1] = "Total"; return(temp)})
 
  colName = switch (method,
                    gaussian = "ES",
                    modified = "MES"
  )
  comp.ES = organizeComponentRisks(getComponentRisk(subs.o, subs.weights, FUN = "ES", n = n, method = method))
  comp.ES.p = lapply(comp.ES, function(x){result = data.frame(x[3,]*100); colnames(result)[1] = colName; return(result)})
  total.ES = lapply(comp.ES, FUN = function(x){temp = data.frame(-100*x[1,1]);  colnames(temp)[1] = row.names(x)[1]; rownames(temp)[1] = "Total";return(temp)})
  
  comp.p = comp.ES.p
  total.p = total.ES
  actWeights = subs.weights[names(comp.p)]
  
  
  for (i in 1:length(comp.p))
  {
    comp.p[[i]] = cbind.data.frame(comp.p[[i]], comp.VaR.p[[i]], comp.sd.p[[i]], ActualWeights = actWeights[[i]]*100)
    
    comp.p[[i]] = switch(method, 
                         gaussian = comp.p[[i]][with(comp.p[[i]], order(-ES, -VaR, -StdDev)),], 
                         modified = comp.p[[i]][with(comp.p[[i]], order(-MES, -MVaR, -StdDev)),])
    
    total.p[[i]] = cbind.data.frame(total.p[[i]], total.VaR[[i]], total.sd[[i]], ActualWeights = c(100))
    row.names(total.p[[i]])[1] = paste0(row.names(total.p[[i]])[1], names(comp.p)[i])
  }
  combined = vector(mode = "list", length = 10)
  for (i in 1:length(comp.p))
  {
    combined[[2*i-1]] = total.p[[i]]
    names(combined)[2*i-1] = names(total.p)[i]
    attr(combined, "subheadings")[2*i-1] = paste(names(comp.p)[i], "total risk", sep = " ")
    combined[[2*i]] = comp.p[[i]]
    names(combined)[2*i] = names(comp.p)[i]
    attr(combined, "subheadings")[2*i] = paste(names(comp.p)[i], "contribution to risk", sep = " ")
  }
  
  return(combined)
}