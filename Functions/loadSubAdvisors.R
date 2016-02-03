loadSubAdvisors = function(){
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  require(xts)
  require(data.table)
  qry = "select * from v_Sub_Returns_Daily_v4 AS v where v.datereported >= '7-1-2009' AND v.Name IS NOT NULL"
  subs = dcast.data.table(data.table(sqlQuery(cn,qry)), formula = DateReported ~ Name, fun.aggregate = mean)
  subs$DateReported = as.Date.factor(subs$DateReported)
  return(as.xts.data.table(subs))
}
