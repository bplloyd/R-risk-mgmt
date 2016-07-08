alphaEWMAContribution_subs = function(subs, weightMode = "historical", lambda = 0.93)
{
  require(xts)
  source('Functions/ewmaVolatilityContribution2.R')
  alpha.alloc.subs = pofAllocations_subs(id = 786, startDate = "2014-01-01", endDate = end(subs),includeMisc = F)
  if(weightMode == "historical")
  {
    return(ewmaVolatilityContribution2(R=subs, weights = alpha.alloc.subs, lambda = lambda, includeMisc = F, curWeightsOnly = F))
  }
  if(weightMode == "current")
  {
    return(ewmaVolatilityContribution2(R=subs, weights = alpha.alloc.subs, lambda = lambda, includeMisc = F, curWeightsOnly = T))
  }
}