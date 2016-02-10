extractFieldFromRoll = function(roll, field){
  require(xts)
  result = matrix(NA, nrow = length(roll), ncol = length(roll[[1]][field][[1]]))
  colnames(result) = names(roll[[1]][field][[1]])
  for (i in 1:length(roll)){
      result[i,] = t(as.vector(roll[[i]][field][[1]]))
  }
  return(xts(result, order.by = as.Date.character(names(roll))))
}