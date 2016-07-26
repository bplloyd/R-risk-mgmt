expectedVolContribution_fund = function(id, subs, lambda, asOfDate = NULL, curWeightsOnly = F)
{
  require(xts)
  source('Functions/lagMF.R')
  source('Functions/getMFnames.R')
  name = switch(as.character(id),
                '774' = 'HHSIX',
                '786' = 'ALPHX',
                '777' = 'MF',
                '782' = 'MN',
                '783' = 'ED',
                '784' = 'LSD',
                '785' = 'LSE'
                )
  if((id == 774) | (id == 786))
  {
    allocs = pofAllocations_subs(id)
  }
  else
  {
    allocs = getAllocations_Rolling()
    allocs = allocs[[name]]
  }
  if(curWeightsOnly)
  {
    if(is.null(asOfDate))
      weights = allocs[nrow(allocs), which(allocs[nrow(allocs),] != 0)]
    else
      weights = allocs[asOfDate, which(allocs[asOfDate,] != 0)]
  }
  else
  {
    if(is.null(asOfDate))
      weights = allocs
    else
      weights = allocs[paste0("/",asOfDate),]
  }
  
  R = subs[, names(weights)]
  R = R[which(rowSums(is.na(R))<ncol(R)),]
  
  if((id == 777) | (id == 786))
  {
    R[, names(R) %in% getMFnames()] = lagMF(R[, names(R) %in% getMFnames()])
  }

  return(ewmaVolatilityContribution2(R = R, weights = weights, lambda = lambda, includeMisc = T, curWeightsOnly = curWeightsOnly))
}