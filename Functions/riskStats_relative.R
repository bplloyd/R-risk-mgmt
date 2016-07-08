riskStats_relative = function(account, width = 63, irWidth = 126)
{
  require(xts)
  require(PerformanceAnalytics)
  require(timeSeries)
  require(XLConnect)
  require(stringr)
  source('Functions/loadMyFuncs.R')
  loadMyFuncs()
  
  #      width = 63
  #      irWidth =126
  #      name = "Coe"
  #      sub = subs[,name]
  #      bms = cbind(sp2, hfrx$HFRXEH) 
  
  account = na.omit(account) 
  nm = names(account)[1]
  
  if(nm %in% c("LSE", "LSD", "MN", "ED", "MF", "ALPIX", "ALPHX", "ALPHA_FSL", "HHSIX", "SPTR"))
  {
    fundId = switch(nm, LSE = 785, LSD = 784, ED = 783, MN = 782, MF = 777, 
                    ALPIX = 786, ALPHA_FSL = 786, ALPHX = 786, HHSIX = 774, SPTR = 999)
  }
  else
  {
    id = getSubID(nm)
    fundId = getFundID(nm)
  }
  
  bms = getBenchmarks(fundId)
  
  lam = switch(as.character(fundId), '785' = 0.94, '784' = 0.91, '783' = 0.92, '782' = 0.95, '777' = 0.98, 
               '786' = 0.92,
               '774' = 0.92)

  
  alpha1 = rollingAlphaBeta(account, bms[,1], width = width)
  alpha2 = rollingAlphaBeta(account, bms[,2], width = width)

  cor1 = sqrt(alpha1[,3])
  cor2 = sqrt(alpha2[,3])
  names(cor1) = paste0("Correlation", "_", names(bms)[1])
  names(cor2) = paste0("Correlation", "_", names(bms)[2])
  
  vol = rollapply(data = account, width = width, FUN = function(r)return(sd(r, na.rm = T)))
  bm1.vol = rollapply(data = bms[,1], width = width, FUN = function(r)return(sd(r, na.rm = T)))
  bm2.vol = rollapply(data = bms[,2], width = width, FUN = function(r)return(sd(r, na.rm = T)))
  
  relVol1 = vol/bm1.vol
  relVol2 = vol/bm2.vol
  names(relVol1) = paste0("RelativeVolatility_", names(bms)[1])
  names(relVol2) = paste0("RelativeVolatility_", names(bms)[2])
  
  ir1 = rollGeomIR(account, bms[,1], width = irWidth)
  ir2 = rollGeomIR(account, bms[,2], width = irWidth)
  names(ir1) = paste0(names(ir1), "_", names(bms)[1])
  names(ir2) = paste0(names(ir2), "_", names(bms)[2])
  

  stats =  cbind(
                    alpha1
                    , cor1
                    , relVol1
                    , ir1
                    , alpha2
                    , cor2
                    , relVol2
                    , ir2
          
  )
  return(stats[rowSums(is.na(stats)) < ncol(stats),])
}







