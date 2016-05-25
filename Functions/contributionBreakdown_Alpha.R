contributionBreakdown_Alpha = function(breakdown, start, end)
{
#     id = 785
#     start = start(uft)
#     end = end(uft)
  if("dplyr" %in% loadedNamespaces()){
    detach("package:dplyr", unload=TRUE)
  }
#   require(RODBC)
#   cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
#   qry = paste0("SELECT
#             v.DateReported
#             , v.Security_Name
#             , v.Security_UID
#             , v.
#           FROM
#               v_FundSecs_FLASHREPORT AS v
#           WHERE
#               v.Fund_UID = 786
#               AND v.DateReported BETWEEN '", start, "' AND '", end, "'
#           ORDER BY
#               v.DateReported")
#   alpha.cont = sqlQuery(cn, qry)
  
  
  pofAllocs = pofAllocations(786)
  pofAllocs = lag(pofAllocs)
  pofAllocs = pofAllocs[paste0(start, "/", end),]
  
  uft.bdowns = lapply(c(777, 782, 783, 784, 785), FUN = function(i)return(contributionBreakdown(i, breakdown = breakdown, start = start, end=end)))
  #contributionBreakdown(786, breakdown = "Asset_Type", start = start, end=end)
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