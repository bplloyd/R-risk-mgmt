loadSubAdvisors = function(){
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  require(xts)
  require(data.table)
  qry = "select * from v_Sub_Returns_Daily_v4 AS v where v.datereported >= '7-1-2009' AND v.Name IS NOT NULL"
  subs = dcast.data.table(data.table(sqlQuery(cn,qry)), formula = DateReported ~ Name, fun.aggregate = mean)
  subs$DateReported = as.Date.factor(subs$DateReported)
  #qry24 = "select v.DateReported, s.abbreviation as Name, v.NAV from hatteras_securitydb.dbo.dailytotalreturn AS v join hamf.subadvisors as s on v.subadvised_uid = s.subadvised_uid where v.datereported >= '3-23-2016' AND v.subadvised_uid IS NOT NULL"
  #subs_navfill = as.xts(dcast.data.table(data.table(sqlQuery(cn,qry24)), formula = DateReported ~ Name, fun.aggregate = mean))
  #res = as.xts.data.table(subs)
  #res24 = CalculateReturns(subs_navfill)['20160324/']
  #res['20160324/', names(res24)] = res24['20160324/']
  return(as.xts.data.table(subs))
}
