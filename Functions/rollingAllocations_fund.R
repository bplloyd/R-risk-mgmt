rollingAllocations_fund = function(allocs_UFT, allocs_Fund)
{
#   rm(out)
#   allocs_UFT = uft.allocations
#   allocs_Fund = alpha.allocs
  require(xts)
  require(PerformanceAnalytics)
  names = as.vector(unlist(lapply(allocs_UFT, FUN = function(x)return(names(x[1])))))
  allocs_Fund = na.omit(allocs_Fund)
  rws = nrow(allocs_Fund)
  out = matrix(data=NA, nrow =  rws, ncol = sum(sapply(allocs_UFT, FUN = function(x)return(NCOL(x)))))
  
  colStart = 1
  for(i in 1:length(allocs_UFT))
  {
     for(j in 1:ncol(allocs_UFT[[i]]))
     {
       out[,colStart] = allocs_UFT[[i]][index(allocs_Fund),j] * allocs_Fund[, names(allocs_UFT)[i]]
       colStart = colStart + 1
     }
  }
  colnames(out) = names
  out = na.fill(out, fill = 0)
  row.names(out) = as.character.Date(index(allocs_Fund))
  return(as.xts(out))
}