rollingVolatilityContribution = function(contribution_bdown, width = 63)
{
  require(xts)
  tslices = createTimeSlices2(contribution_bdown, initialWindow = width)
  return(as.xts(t(sapply(tslices, FUN = function(t)return(volatilityContribution(contribution_bdown[t,]))))))
}