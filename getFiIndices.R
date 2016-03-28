getFiIndices = function(inds)
{
  fi = inds[, c(grep("H0A", names(inds)), grep("C0A0", names(inds)), grep("B0A0", names(inds)), grep("LBUSTRUU", names(inds)))]
  fi = fi[-which(rowSums(is.na(fi))==ncol(fi)),]
  fi = CalculateReturns(fi)
  return(fi)
}