currentLevelsSubs = function(subs.o, FUN = "VaR", p=0.99, width = 126, method = "gaussian")
{
  levels = list(
    LSE = apply(subs.o$LSE, 2, function(x)compareLevels(as.xts(x), FUN = FUN, p = p, width = width, method = method)),
    LSD = apply(subs.o$LSD, 2, function(x)compareLevels(as.xts(x), FUN = FUN, p = p, width = width, method = method)),
    ED = apply(subs.o$ED, 2, function(x)compareLevels(as.xts(x), FUN = FUN, p = p, width = width, method = method)),
    MN = apply(subs.o$MN, 2, function(x)compareLevels(as.xts(x), FUN = FUN, p = p, width = width, method = method)),
    MF = apply(subs.o$MF, 2, function(x)compareLevels(as.xts(x), FUN = FUN, p = p, width = width, method = method))
  )
  result = lapply(levels, FUN = function(l)return(as.data.frame(matrix(unlist(l), nrow = length(l), byrow = T, dimnames = list(names(l), colnames(l[[1]]))))))
  names(result) = paste(names(result), FUN, p, sep = " ")
  return(result)
}

