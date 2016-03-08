applySpecificRisk = function(accounts, sp, hy, n=NULL, reportDate = NULL)
{
    if(is.null(reportDate))
        reportDate = end(subs.o$LSE)
    subs.o = lapply(subs.o, function(x)return(x[paste0("/",reportDate)]))
    sp = sp[paste0("/",reportDate)]
    hy = hy[paste0("/",reportDate)]
    if(is.list(accounts)){
      if(!is.null(n))
          mapply(function(subs, bm)return(table.SpecificRisk(subs[(nrow(subs)-n+1):nrow(subs),], bm[(length(bm)-n+1):length(bm)])*100), subs.o, list(sp, sp, hy, sp, sp))
      else
        mapply(function(subs, bm)return(table.SpecificRisk(subs, bm)*100), subs.o, list(sp, sp, hy, sp, sp))
    }
    else{
        if(ncol(accounts)==5)
            
    }
}