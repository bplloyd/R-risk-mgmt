extractSingleSDroll = function(Fm.SD.roll, name, measure){
  require(xts)
  result = matrix(NA, nrow = length(Fm.SD.roll), ncol = ncol(Fm.SD.roll[[1]][paste(measure)][[1]])) 
  colnames(result) = colnames(Fm.SD.roll[[1]][paste(measure)][[1]])
  for (i in 1:length(Fm.SD.roll)){
    
    if(name %in% row.names(Fm.SD.roll[[i]][paste(measure)][[1]])){
      result[i,] = Fm.SD.roll[[i]][paste(measure)][[1]][c(name),]
    }
  }
  return(xts(result, order.by = as.Date.character(names(Fm.SD.roll))))
}