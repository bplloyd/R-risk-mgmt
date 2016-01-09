loadSubAdvisors = function(startDate){
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  require(xts)
  subs = sqlQuery(cn,"select * from v_Sub_Returns_Daily_Pivoted AS v where v.datereported >= '7-1-2009' order by v.datereported")
  subs = as.xts(subs[,2:25], order.by = as.Date.POSIXct(subs$DateReported))
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
