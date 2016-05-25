pofAllocations_subs_singleDay = function(id=786, date, includeMisc = T)
{

  uftAllocs = getAllocations(date)
  pofAllocs = pofAllocations(id)
  pofAllocs = pofAllocs[date,]
  
  pofAllocs_subs = lapply(names(uftAllocs), FUN = function(n)return(data.frame(Name = uftAllocs[[n]]$Name, Allocation = uftAllocs[[n]]$Allocation*as.vector(pofAllocs[1,n])[1])))
  names(pofAllocs_subs) = names(uftAllocs)
  return(pofAllocs_subs)
  
  
  
#   ncols = switch(as.character(includeMisc), 
#                  "FALSE" = sum(sapply(uftAllocs, function(a)return(ncol(a[,-grep("Misc", names(a))])))),
#                  "TRUE" = sum(sapply(uftAllocs, function(a)return(ncol(a))))
#   )
#   
#   allocs = matrix(data = NA, ncol = ncols, nrow = nrow(uftAllocs[[1]]))
#   rownames(allocs) = as.character.Date(index(pofAllocs))
#   colnames(allocs) = switch(as.character(includeMisc), 
#                             "FALSE" = unlist(sapply(uftAllocs, function(a)return(names(a[,-grep("Misc", names(a))])))),
#                             "TRUE" = unlist(sapply(uftAllocs, function(a)return(names(a))))
#   )
#   startCol = 0
#   for(i in 1:length(uftAllocs))
#   {
#     endCol = startCol + switch(as.character(includeMisc), 
#                                "FALSE" = ncol(uftAllocs[[i]][,-grep("Misc", names(uftAllocs[[i]]))]),
#                                "TRUE" = ncol(uftAllocs[[i]])
#     )
#     allocs[,(startCol+1):endCol] = switch(as.character(includeMisc), 
#                                           "FALSE" = t(sapply(index(uftAllocs[[i]][,-grep("Misc", names(uftAllocs[[i]]))]),  
#                                                              FUN = function(x)return(pofAllocs[x, names(uftAllocs)[i]]*as.vector(uftAllocs[[i]][x,-grep("Misc", names(uftAllocs[[i]]))]))
#                                           )
#                                           ),
#                                           "TRUE" = t(sapply(index(uftAllocs[[i]]),  
#                                                             FUN = function(x)return(pofAllocs[x, names(uftAllocs)[i]]*as.vector(uftAllocs[[i]][x,]))
#                                           )
#                                           )
#     )
#     startCol = endCol
#   }
#   return(xts(allocs, order.by = ymd(row.names(allocs))))
}