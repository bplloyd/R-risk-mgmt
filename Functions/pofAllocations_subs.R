pofAllocations_subs = function(id, includeMisc = T)
{
#   id = 786
#   startDate = as.Date("2014/12/31")
#   endDate = as.Date("2016/04/29")
  require(lubridate)
  require(xts)
  
  uftAllocs = getAllocations_Rolling()
  pofAllocs = pofAllocations(id)
  maxStart = max(start(uftAllocs$LSE), start(pofAllocs))
  minEnd = min(end(uftAllocs$LSE), end(pofAllocs))
  pofAllocs = pofAllocs[paste0(maxStart, "/", minEnd),]
  uftAllocs = lapply(uftAllocs, FUN = function(u)return(u[paste0(maxStart, "/", minEnd),]))
  
#   ncols = switch(as.character(includeMisc), 
#                  "FALSE" = sum(sapply(uftAllocs, function(a)return(ncol(a[,-grep("Misc", names(a))])))),
#                  "TRUE" = sum(sapply(uftAllocs, function(a)return(ncol(a))))
#   )
  ncols = sum(sapply(uftAllocs, function(a)return(ncol(a))))

  allocs = matrix(data = NA, ncol = ncols, nrow = nrow(uftAllocs$LSE))
  rownames(allocs) = as.character.Date(index(pofAllocs))
  colnames(allocs) = unlist(sapply(uftAllocs, function(a)return(names(a))))
                          
  startCol = 0
  for(i in 1:length(uftAllocs))
  {
    endCol = startCol + ncol(uftAllocs[[i]])
    maxStart_i = which(rownames(allocs) == max(maxStart, start(uftAllocs[[i]])))
    minEnd_i = which(rownames(allocs) == min(minEnd, end(uftAllocs[[i]])))
    
    allocs[maxStart_i:minEnd_i,(startCol+1):endCol] = t(sapply(index(uftAllocs[[i]]),  
                                                            FUN = function(x)return(pofAllocs[x, names(uftAllocs)[i]]*as.vector(uftAllocs[[i]][x,]))
                                                            )
                                                      )
                                          
    startCol = endCol
  }
  if(includeMisc)
  {
      return(xts(allocs, order.by = ymd(row.names(allocs))))
  }
  if(!includeMisc)
  {
      miscCols = grep("Misc", colnames(allocs))
      
  }
}