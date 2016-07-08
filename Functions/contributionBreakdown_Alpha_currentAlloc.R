contributionBreakdown_Alpha_currentAlloc = function(breakdown, start, end)
{
  #     id = 785
  #     start = start(uft)
  #     end = end(uft)
  if("dplyr" %in% loadedNamespaces()){
    detach("package:dplyr", unload=TRUE)
  }
  start = "2015-07-0"
  end = "2016-04-29"
  breakdown = "Asset_Type"
  currentAllocs = pofAllocations_subs_singleDay(id=786, date = end)
  
  
  sub.bdowns = lapply(currentAllocs, 
                      FUN = function(x){
                        l = lapply(
                              x[1]$Name, 
                              FUN = function(n)return(
                                contributionBreakdown(
                                  getSubID(n), 
                                  breakdown = breakdown, 
                                  start = start, 
                                  end = end
                                )
                              )
                            );
                        names(l) = x[1]$Name;
                        return(l)
                      }
                    )
  for(i in 1:length(sub.bdowns$MF))
  {
    sub.bdowns$MF[[i]] = xts(rowSums(sub.bdowns$MF[[i]]), order.by = index(sub.bdowns$MF[[i]]))
    names(sub.bdowns$MF[[i]]) = "MF"
  }
             
  names(uft.bdowns) = c("MF", "MN", "ED", "LSD", "LSE")
  alpha.cats= unique(unlist(lapply(uft.bdowns, FUN = function(b)return(names(b[1])))))
  
  if((breakdown != "SubAdvisor") & (breakdown != "Fund_UID")){
    alpha.cats = c(alpha.cats, "MF")
  }
  
  alpha.bdown = matrix(data = NA, nrow = nrow(uft.bdowns[[1]]), ncol = length(alpha.cats))
  colnames(alpha.bdown) = alpha.cats
  
  if((breakdown != "SubAdvisor") & (breakdown != "Fund_UID")){
    alpha.bdown[,"MF"] = xts(rowSums(uft.bdowns$MF), order.by = index(uft.bdowns$MF))*pofAllocs$MF
    
    for(i in 2:length(uft.bdowns)){
      for(j in 1:(ncol(alpha.bdown)-1)){
        if(colnames(alpha.bdown)[j] %in% names(uft.bdowns[[i]])){
          alpha.bdown[, j] = na.fill(alpha.bdown[,j],fill = 0) + pofAllocs[, names(uft.bdowns)[i]] * uft.bdowns[[i]][,colnames(alpha.bdown)[j]] 
        }
      }
    }
  }
  if((breakdown == "SubAdvisor") | (breakdown == "Fund_UID")){
    for(i in 1:length(uft.bdowns)){
      for(j in 1:ncol(alpha.bdown)){
        if(colnames(alpha.bdown)[j] %in% names(uft.bdowns[[i]])){
          alpha.bdown[, j] = na.fill(alpha.bdown[,j],fill = 0) + pofAllocs[, names(uft.bdowns)[i]] * uft.bdowns[[i]][,colnames(alpha.bdown)[j]] 
        }
      }
    }
  }
  
  
  
  return(xts(alpha.bdown, order.by = index(pofAllocs)))
}