getWeights = function(subs.o = NULL, asOfDate=NULL, includeMisc = F)
{
  allocations = getAllocations(asOfDate)
  if(!is.null(subs.o)){
    allocations$MN = allocations$MN[which(allocations$MN$Name %in% names(subs.o$MN)),]
    allocations$LSE = allocations$LSE[which(allocations$LSE$Name %in% names(subs.o$LSE)),]
    allocations$LSD = allocations$LSD[which(allocations$LSD$Name %in% names(subs.o$LSD)),]
    allocations$ED = allocations$ED[which(allocations$ED$Name %in% names(subs.o$ED)),]
    allocations$MF = allocations$MF[which(allocations$MF$Name %in% names(subs.o$MF)),]
  }
  
  if(includeMisc)
    return(lapply(allocations, function(x){w =  as.vector(x$Allocation); names(w) = as.vector(x$Name); return(w/sum(w))}))
  if(!includeMisc)
    return(lapply(allocations, function(x){w =  as.vector(x$Allocation); names(w) = as.vector(x$Name);  w = w[-grep("Misc", names(w))]; return(w/sum(w))}))
}