getPosReturns_db = function(Cus)
{
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  qry = paste0("SELECT DISTINCT v.DateReported, v.PercPriceChange FROM v_SubSecs_FLASHREPORT AS v WHERE v.Cusip = '", Cus, "' AND v.PercPriceChange IS NOT NULL ORDER BY v.DateReported")
  res = sqlQuery(cn, qry)
  res = xts(res[,2], order.by = as.Date(res$DateReported))
  index(res) = as.Date(index(res))
  return(res)
  
}