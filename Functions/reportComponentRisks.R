reportComponentRisks = function(subs.o, subs.weights, n = 126)
{
  comp.sd = organizeComponentRisks(getComponentRisk(subs.o, subs.weights, FUN = "StdDev", n = n))
  comp.sd.p = lapply(comp.sd, function(x){result = data.frame(x[3,]*100); colnames(result)[1] = "StdDev"; return(result)})
  total.sd = lapply(comp.sd, FUN = function(x){temp = data.frame(x[1,1]*100);   colnames(temp)[1] = row.names(x)[1]; rownames(temp)[1] = "Total"; return(temp)})
  
  comp.VaR = organizeComponentRisks(getComponentRisk(subs.o, subs.weights, FUN = "VaR", n = n))
  comp.VaR.p = lapply(comp.VaR, function(x){result = data.frame(x[3,]*100); colnames(result)[1] = "MVaR"; return(result)})
  total.VaR = lapply(comp.VaR, FUN = function(x){temp = data.frame(x[1,1]*100);   colnames(temp)[1] = row.names(x)[1]; rownames(temp)[1] = "Total"; return(temp)})
 
  comp.ES = organizeComponentRisks(getComponentRisk(subs.o, subs.weights, FUN = "ES", n = n))
  comp.ES.p = lapply(comp.ES, function(x){result = data.frame(x[3,]*100); colnames(result)[1] = "MES"; return(result)})
  total.ES = lapply(comp.ES, FUN = function(x){temp = data.frame(x[1,1]*100);  colnames(temp)[1] = row.names(x)[1]; rownames(temp)[1] = "Total";return(temp)})
  
  comp.p = comp.ES.p
  total.p = total.ES
  
  for (i in 1:length(comp.p))
  {
    comp.p[[i]] = cbind.data.frame(comp.p[[i]], comp.VaR.p[[i]], comp.sd.p[[i]])
    comp.p[[i]] = comp.p[[i]][with(comp.p[[i]], order(-MES, -MVaR, -StdDev)),]
    
    total.p[[i]] = cbind.data.frame(total.p[[i]], total.VaR[[i]], total.sd[[i]])
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