contributionBreakdown = function(id, breakdown, start, end)
{
  contribution = contributionSummary(id, start, end)
  tbl = summarise(group_by_(contribution, quote(DateReported), breakdown), Contribution = sum(Contribution_Total, na.rm = T))
  tbl=dcast(tbl, formula = paste("DateReported", breakdown, sep = " ~ "))
  for(i in 2:ncol(tbl))
    tbl[which(is.na(tbl[,i,with=F])),i] = 0
  return(as.xts.data.table(tbl))
}