contributionBreakdown3 = function(contributionSummary, breakdown)
{
  tbl = summarise(group_by_(contributionSummary, quote(DateReported), breakdown), Contribution = sum(Contribution_Total, na.rm = T))
  tbl=dcast(tbl, formula = paste("DateReported", breakdown, sep = " ~ "))
  for(i in 2:ncol(tbl))
    tbl[which(is.na(tbl[,i,with=F])),i] = 0
  res = as.xts.data.table(tbl)
  if(length(which(colSums(res==0) == nrow(res)))>0){
   res = res[,-which(colSums(res==0) == nrow(res))]
  }
  return(res)
}