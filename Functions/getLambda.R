getLambda = function(nm)
{
  if(nm %in% c("LSE", "LSD", "MN", "ED", "MF", "ALPIX", "ALPHX", "ALPHA_FSL", "HHSIX", "SPTR"))
  {
    fundId = switch(nm, LSE = 785, LSD = 784, ED = 783, MN = 782, MF = 777, 
                    ALPIX = 786, ALPHA_FSL = 786, ALPHX = 786, HHSIX = 774, SPTR = 999)
  }
  else
  {
    fundId = getFundID(nm)
  }
  
  #bms = getBenchmarks(fundId)
  
  return(switch(as.character(fundId), '785' = 0.93, '784' = 0.91, '783' = 0.92, '782' = 0.95, '777' = 0.98, 
               '786' = 0.92,
               '774' = 0.92, '999' = 0.93))
}