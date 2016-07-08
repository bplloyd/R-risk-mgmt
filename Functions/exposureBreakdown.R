exposureBreakdown = function(id, breakdown, start, end)
{
  #     id = 785
  #     start = start(uft)
  #     end = end(uft)
  require(data.table)
  require(dplyr)
  exposure = exposureSummary(id, start, end)
  tbl = summarise(group_by_(exposure, quote(DateReported), breakdown), Exposure = sum(Exposure, na.rm = T))
  tbl=dcast(tbl, formula = paste("DateReported", breakdown, sep = " ~ "))
  for(i in 2:ncol(tbl))
    tbl[which(is.na(tbl[,i,with=F])),i] = 0
  res = as.xts.data.table(tbl)
  if(length(which(colSums(res==0) == nrow(res)))>0){
    res = res[,-which(colSums(res==0) == nrow(res))]
  }
  return(res)
}