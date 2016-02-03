loadIndices = function(){
  require(RODBC)
  require(data.table)
  require(xts)
  require(PerformanceAnalytics)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  sql = "SELECT CAST(v.DateReported AS date) 'DateReported', v.Ticker, v.PX_LAST FROM v_Index_Px_Daily AS v"
  indices = dcast(data.table(sqlQuery(cn, sql)), formula = DateReported ~ Ticker, fun.aggregate = mean)
  indices$DateReported = as.Date.factor(indices$DateReported)
  return(as.xts.data.table(indices))
}
