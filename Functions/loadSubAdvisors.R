loadSubAdvisors = function(){
  require(RODBC)
  connStr = "driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true"
  cn = odbcDriverConnect(connStr)
  require(PerformanceAnalytics)
  require(xts)
  require(data.table)
  qry = "SELECT
            v.DateReported
            , s.Abbreviation 'Name'
            , v.NAV
          FROM 
            HAMF.SUB_NAV AS v 
            JOIN HAMF.SubAdvisors AS s ON s.SubAdvised_UID = v.SubAdvised_UID
          WHERE
            v.DateReported >= '2009-06-30'
            AND s.Abbreviation IS NOT NULL"
  
  subs = dcast.data.table(data.table(sqlQuery(cn,qry)), formula = DateReported ~ Name, fun.aggregate = mean)
  subs$DateReported = as.Date.factor(subs$DateReported)
  #qry24 = "select v.DateReported, s.abbreviation as Name, v.NAV from hatteras_securitydb.dbo.dailytotalreturn AS v join hamf.subadvisors as s on v.subadvised_uid = s.subadvised_uid where v.datereported >= '3-23-2016' AND v.subadvised_uid IS NOT NULL"
  #subs_navfill = as.xts(dcast.data.table(data.table(sqlQuery(cn,qry24)), formula = DateReported ~ Name, fun.aggregate = mean))
  #res = as.xts.data.table(subs)
  #res24 = CalculateReturns(subs_navfill)['20160324/']
  #res['20160324/', names(res24)] = res24['20160324/']
  res = as.xts.data.table(subs)
  res = CalculateReturns(res)
  res$WhiteOak['20151016/'] = NaN
  res$MiscMN['201507'] = 0
  res$MiscMN['20150903'] = 0
  res$MiscMN['20151012'] = 0
  return(res)
}
