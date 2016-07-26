reportVolatilityContribution = function(id, name, endDate = NULL)
{
  
  lam = getLambda(name)
  fundId = getFundID(name)

  if(is.na(fundId))
  {
    fundId = id
  }
  
  contribution.summary = contributionSummary3(id = id, end = endDate)
  exposure.summary = exposureSummary2(id = id, end = endDate)
  
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
                FUN = function(b){
                                    if(b == "SubAdvisor")
                                    {
                                      if((id == 786) | (id == 774))
                                      {
                                        exp = pofAllocations_subs(id, includeMisc = T)
                                      }
                                      else
                                      {
                                        exp = getAllocations_Rolling()
                                        
                                        uf = switch(EXPR = as.character(id),
                                               '777' = 'MF',
                                               '782' = 'MN',
                                               '783' = 'ED', 
                                               '784' = 'LSD', 
                                               '785' = 'LSE')
                                        exp = exp[[uf]]
                                      }
                                    }
                                    else if(b == "Fund")
                                    {
                                      exp = pofAllocations(id)
                                      exp = exp[,-which(names(exp)=="STIC")]
                                    }
                                    else
                                    {
                                      exp = exposureBreakdown2(exposureSummary = exposure.summary,
                                                                    breakdown = b
                                      )
                                    };
                                    
                                    if(!is.null(endDate))
                                      {exp = exp[paste0("/", endDate),]};
                                    
                                    return(list(
                                              VolatilityContribution = ewmaVolatilityContribution(
                                                    rtn = contributionBreakdown3(contribution.summary, 
                                                          breakdown = b
                                                          ),
                                                    lambda = lam
                                                                  ),
                                              WeightedExposure = weightedAverageExposure(
                                                    exposure = exp,
                                                    lambda = lam,
                                                    rolling = T
                                                                  )
                                              )
                                            )}
                                        
                )
  return(res)
}