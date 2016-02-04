loadPOFs = function(){
  require(RODBC)
  require(magrittr)
  require(xts)
  require(PerformanceAnalytics)
  connStr = "driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true"
  cn = odbcDriverConnect(connStr)
  qry = "WITH 
	Dates AS
  (
  select distinct 
  cast(datereported as date) 'DateReported'
  from 
  hatteras_securitydb.dbo.dailytotalreturn
  )
  SELECT
    d.DateReported
    , p.ALPHX
    , p.ALPIX
    , p.HHSIX
    , p.HFINX
    , p.HLSIX
    , p.HMFIX
  FROM
  Dates AS d
  JOIN
  (
  select 
  CAST(n.DateReported AS datetime) 'DateReported'
  , c.Symbol
  , n.NAV_Adjusted
  from 
  hamf.pof_nav AS n
  LEFT JOIN hatteras_Securitydb.dbo.fundclass AS c ON n.Fund_UID = c.Fund_UID and n.FundClass_UID = c.FundClass_UID	
  where
  n.FundClass_UId <> 99
  ) AS A
  PIVOT(MAX(A.NAV_Adjusted) FOR A.Symbol IN ([ALPHX],[ALPIX],[HHSIX],[HFINX],[HLSIX], [HMFIX])) AS P ON d.DateReported = P.DateReported
  ORDER BY d.DateReported"
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