loadPOFs = function(){
  require(RODBC)
  require(magrittr)
  require(xts)
  require(PerformanceAnalytics)
  connStr = "driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true"
  cn = odbcDriverConnect(connStr)
  qry = "SELECT
            v.DateReported
            , CASE v.Fund_UID
                 WHEN 777 THEN 'MF'
                 WHEN 782 THEN 'MN'
                 WHEN 783 THEN 'ED'
                 WHEN 784 THEN 'LSD'
                 WHEN 785 THEN 'LSE'
              END 'Name'
            , v.NAV_Backfilled 'NAV'
          FROM 
            HAMF.UFT_NAV AS v 
          WHERE
            v.DateReported >= '2009-06-30'"
  
  
  pofs = sqlQuery(cn,qry)
  pofs= as.xts(pofs[,2:7], order.by = as.Date.character(pofs$DateReported))
  maxDate = as.Date.character(end(pofs), format = '%Y-%m-%d')

  qry2 = "SELECT CAST(MAX(d.DateReported) AS date) 'DateReported' FROM Hatteras_SecurityDB.dbo.DailyTotalReturn AS d"
  maxDateDTR = sqlQuery(cn,qry2)
  maxDateDTR = as.Date.factor(maxDateDTR[[1]])
  pofs = pofs %>% CalculateReturns()
  if(maxDate!=maxDateDTR){
    qry3 = "SELECT CAST(d.DateReported as date) 'DateReported', c.Symbol, d.OneDayReturn/100 'OneDayReturn' FROM Hatteras_SecurityDB.dbo.DailyTotalReturn AS d LEFT JOIN hatteras_Securitydb.dbo.fundclass AS c ON d.Fund_UID = c.Fund_UID and d.FundClass_UID = c.FundClass_UID WHERE d.DateReported > '"
    qry3 = paste(qry3, maxDate, sep = ' ')
    qry3 = paste(qry3,"') as A",  sep = ' ' )
    qry3 = paste("SELECT p.* FROM (", qry3, sep = '')
    qry3 = paste(qry3, "PIVOT(MAX(a.OneDayReturn) FOR a.SYMBOL IN ([ALPHX],[ALPIX],[HHSIX],[HFINX],[HLSIX], [HMFIX])) AS p ORDER BY p.DateReported", sep = ' ')
    pofs2 = sqlQuery(cn, qry3)
    pofs = rbind.xts(pofs, as.xts(pofs2[,names(pofs)], order.by = as.Date.character(pofs2$DateReported)))
    }
  return(pofs)
}