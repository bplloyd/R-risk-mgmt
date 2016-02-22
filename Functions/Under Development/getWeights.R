getWeights = function(subs.o = NULL, asOfDate=NULL, includeMisc = F)
{
  allocations = getAllocations(asOfDate)
  if(includeMisc)
    return(lapply(allocations, function(x){w =  as.vector(x$Allocation); names(w) = as.vector(x$Name); return(w)}))
  if(!includeMisc)
    return(lapply(allocations, function(x){w =  as.vector(x$Allocation); names(w) = as.vector(x$Name);  w = w[-grep("Misc", names(w))]; return(w/sum(w))}))
}