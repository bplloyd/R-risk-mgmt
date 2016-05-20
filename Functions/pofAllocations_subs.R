pofAllocations_subs = function(id, startDate, endDate, includeMisc = F)
{
#   id = 786
#   startDate = as.Date("2014/12/31")
#   endDate = as.Date("2016/04/29")
  require(lubridate)
  require(xts)
  
  uftAllocs = getAllocations_Rolling(start= startDate, end=endDate)
  pofAllocs = pofAllocations(id)
  pofAllocs = pofAllocs[paste0(startDate, "/", endDate)]
  
  ncols = switch(as.character(includeMisc), 
                 "FALSE" = sum(sapply(uftAllocs, function(a)return(ncol(a[,-grep("Misc", names(a))])))),
                 "TRUE" = sum(sapply(uftAllocs, function(a)return(ncol(a))))
  )
  
  allocs = matrix(data = NA, ncol = ncols, nrow = nrow(uftAllocs[[1]]))
  rownames(allocs) = as.character.Date(index(pofAllocs))
  colnames(allocs) = switch(as.character(includeMisc), 
                            "FALSE" = unlist(sapply(uftAllocs, function(a)return(names(a[,-grep("Misc", names(a))])))),
                            "TRUE" = unlist(sapply(uftAllocs, function(a)return(names(a))))
                            )
  startCol = 0
  for(i in 1:length(uftAllocs))
  {
    endCol = startCol + switch(as.character(includeMisc), 
                               "FALSE" = ncol(uftAllocs[[i]][,-grep("Misc", names(uftAllocs[[i]]))]),
                               "TRUE" = ncol(uftAllocs[[i]])
    )
    allocs[,(startCol+1):endCol] = switch(as.character(includeMisc), 
                                          "FALSE" = t(sapply(index(uftAllocs[[i]][,-grep("Misc", names(uftAllocs[[i]]))]),  
                                                             FUN = function(x)return(pofAllocs[x, names(uftAllocs)[i]]*as.vector(uftAllocs[[i]][x,-grep("Misc", names(uftAllocs[[i]]))]))
                                                             )
                                                      ),
                                          "TRUE" = t(sapply(index(uftAllocs[[i]]),  
                                                            FUN = function(x)return(pofAllocs[x, names(uftAllocs)[i]]*as.vector(uftAllocs[[i]][x,]))
                                                            )
                                                      )
                                          )
    startCol = endCol
  }
  return(xts(allocs, order.by = ymd(row.names(allocs))))
}