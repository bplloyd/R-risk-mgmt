rollingVolatilityContribution = function(contribution_breakdown, width = 63)
{
  #contribution_breakdown = coe.cbd_sector
  tslices = createTimeSlices2(data = contribution_breakdown, initialWindow = width, fixedWindow = T, on = "days")
  return(as.xts(t(sapply(tslices, FUN = function(t)return(volatilityContribution(contribution_breakdown[t, ]))))))
}


ewmaVolatilityContribution = function(contribution_breakdown, lambda)
{
  contribution_breakdown = coe.cbd_sector
  tslices = createTimeSlices2(data = contribution_breakdown, initialWindow = width, fixedWindow = T, on = "days")
  return(as.xts(t(sapply(tslices, FUN = function(t)return(volatilityBreakdown(contribution_breakdown[t, ]))))))
}