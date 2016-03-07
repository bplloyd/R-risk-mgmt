reportCorrelation = function(subs.o, bm, dates, width = 126)
{
  result = list(AvgCorToSp = reportAvgCorToBM(subs.o, bm, dates, width = width), AvgCor = reportAvgCorrelation(dates, subs.o, n = width))
  attr(result, "subheadings") = c("Average Correlation to SP", "Average Correlation between SubAdvisors")
  return(result)
}
