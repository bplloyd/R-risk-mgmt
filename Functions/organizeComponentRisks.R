organizeComponentRisks = function(compRisks)
{
    temp = lapply(compRisks, function(x)t(as.data.frame(x)))
    return(temp)
}
  

