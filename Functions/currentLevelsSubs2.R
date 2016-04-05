currentLevelsSubs2 = function(subs.o, FUN = "VaR", p=0.98, width = 126, method = "modified", omissions = NULL, reportDate = NULL, mode = "change")
{
  if(is.null(reportDate)){reportDate = end(subs.o$LSE)}

  levels = list(
    LSE = apply(subs.o$LSE[paste("/", reportDate, sep = ""), which(!(names(subs.o$LSE) %in% omissions))], 2, function(x)compareLevels2(as.xts(x), FUN = FUN, p = p, width = width, method = method, mode = mode)),
    LSD = apply(subs.o$LSD[paste("/", reportDate, sep = ""), which(!(names(subs.o$LSD) %in% omissions))], 2, function(x)compareLevels2(as.xts(x), FUN = FUN, p = p, width = width, method = method, mode = mode)),
    ED = apply(subs.o$ED[paste("/", reportDate, sep = ""), which(!(names(subs.o$ED) %in% omissions))], 2, function(x)compareLevels2(as.xts(x), FUN = FUN, p = p, width = width, method = method, mode = mode)),
    MN = apply(subs.o$MN[paste("/", reportDate, sep = ""), which(!(names(subs.o$MN) %in% omissions))], 2, function(x)compareLevels2(as.xts(x), FUN = FUN, p = p, width = width, method = method, mode = mode)),
    MF = apply(subs.o$MF[paste("/", reportDate, sep = ""), which(!(names(subs.o$MF) %in% omissions))], 2, function(x)compareLevels2(as.xts(x), FUN = FUN, p = p, width = width, method = method, mode = mode))
  )
  result = lapply(levels, FUN = function(l)return(as.data.frame(matrix(unlist(l), nrow = length(l), byrow = T, dimnames = list(names(l), colnames(l[[1]]))))))
  names(result) = paste(names(result), FUN, p, sep = " ")
  return(result)
}

