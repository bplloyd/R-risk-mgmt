loadSubAdvisors = function(){
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  require(xts)
  subs = sqlQuery(cn,"select * from v_Sub_Returns_Daily_Pivoted AS v where v.datereported >= '7-1-2009' order by v.datereported")
  subs = as.xts(subs[,2:25], order.by = as.Date.POSIXct(subs$DateReported))
  qry2 = "SELECT CAST(MAX(d.DateReported) AS date) 'DateReported' FROM Hatteras_SecurityDB.dbo.SubSecLog AS d"
  maxDateSecLog = sqlQuery(cn,qry2)
  maxDateSecLog = as.Date.factor(maxDateSecLog[[1]])
  maxDate = as.Date.character(end(subs), format = '%Y-%m-%d')
  if(maxDate!=maxDateSecLog){
    qry3 = "SELECT CAST(d.DateReported as date) 'DateReported', c.Symbol, d.OneDayReturn/100 'OneDayReturn' FROM Hatteras_SecurityDB.dbo.DailyTotalReturn AS d LEFT JOIN hatteras_Securitydb.dbo.fundclass AS c ON d.Fund_UID = c.Fund_UID and d.FundClass_UID = c.FundClass_UID WHERE d.DateReported > '"
    qry3 = paste(qry3, maxDate, sep = ' ')
    qry3 = paste(qry3,"') as A",  sep = ' ' )
    qry3 = paste("SELECT p.* FROM (", qry3, sep = '')
    qry3 = paste(qry3, "PIVOT(MAX(a.OneDayReturn) FOR a.SYMBOL IN ([ALPHX],[ALPIX],[HHSIX],[HFINX],[HLSIX], [HMFIX])) AS p ORDER BY p.DateReported", sep = ' ')
    pofs2 = sqlQuery(cn, qry3)
    pofs = rbind.xts(pofs, as.xts(pofs2[,names(pofs)], order.by = as.Date.character(pofs2$DateReported)))
  }
  ed.subs = subs[,1:4]
  lse.subs = subs[,5:10]
  mn.subs = subs[,11:13]
  lsd.subs = subs[,14:20]
  mf.subs = subs[,21:24]
  names(ed.subs) = c('Front4', 'Moab', 'Mntr', 'WOak')
  names(lse.subs) = c('Coe', 'ISF', 'APIS', 'BlueJay', 'BoardmanBay', 'Lorem')
  names(mn.subs) = c('Jadwin', 'Longbow', 'Nicholas')
  names(lsd.subs) = c('Lutetium', 'Matlin', 'MiscII', 'Phoenix', 'RavenRock', 'Soundpoint', 'SmithBreeden')
  names(mf.subs) = c('Centurion', 'Dominion', 'Revolution', 'Row')
  return(merge.xts(ed.subs, mn.subs, lsd.subs, lse.subs, mf.subs))
}
