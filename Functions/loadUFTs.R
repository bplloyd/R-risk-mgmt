loadUFTs = function(){
  require(RODBC)
  require(magrittr)
  require(xts)
  require(PerformanceAnalytics)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  
  ufts = sqlQuery(cn,"select p.DateReported, p.[777] 'MF', p.[782] 'MN', p.[783] 'ED', p.[784] 'LSD', p.[785] 'LSE' from 
(
  select CAST(v.DateReported as datetime) 'DateReported', v.Fund_UID, v.NAV from 
  HAMF.UFT_NAV as v
  ) AS a
  PIVOT(max(a.Nav) FOR a.FUnd_UID IN ([777], [782], [783], [784], [785])) as P
  order by P.DateReported")
  ufts = as.xts(ufts[,2:6], order.by = as.Date.POSIXct(ufts$DateReported))
  #names(ufts) = c('MF', 'MN', 'ED', 'LSD', 'LSE')
  return(ufts %>% CalculateReturns())
}