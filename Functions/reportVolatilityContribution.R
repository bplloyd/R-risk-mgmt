reportVolatilityContribution = function(id, name)
{
  
  lam = getLambda(name)
  fundId = getFundID(name)

  if(is.na(fundId))
  {
    fundId = id
  }
  
  contribution.summary = contributionSummary3(id = id)
  exposure.summary = exposureSummary2(id = id)
  
  if(fundId %in% c(782, 783, 785))
  {
    breakdowns = c("Sector", "Mkt_Cap", "Country", "HAMF_TYPE")
  }
  
  if(fundId == 783)
  {
    breakdowns = c("Sector", "Mkt_Cap", "Country", "HAMF_TYPE", "SP_Rating")
  }
  
  if(fundId == 784)
  {
    breakdowns = c("Sector", "Country", "HAMF_TYPE", "SP_Rating")
  }
  
  if(fundId == 777)
  {
    breakdowns = c("HAMF_TYPE")
  }
  
  if(fundId %in% c(786, 774))
  {
    breakdowns = c("Sector", "Country", "Mkt_Cap", "HAMF_TYPE", "SP_Rating", "Fund")
  }
  
  if(id > 700)
  {
    breakdowns = c(breakdowns, "SubAdvisor")
  }
  
  names(breakdowns) = breakdowns
  res = lapply(breakdowns, 
                FUN = function(b)return(list(
                                              VolatilityContribution = ewmaVolatilityContribution(
                                                    rtn = contributionBreakdown3(contribution.summary, 
                                                          breakdown = b
                                                          ),
                                                    lambda = lam
                                                                  ),
                                              WeightedExposure = weightedAverageExposure(
                                                    exposure = exposureBreakdown2(exposureSummary = exposure.summary,
                                                                                  breakdown = b
                                                                                  ),
                                                    lambda = lam,
                                                    rolling = T
                                                                  )
                                              )
                                            )
                                        
                )
  return(res)
}