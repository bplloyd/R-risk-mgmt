loadPOFs = function(){
  require(RODBC)
  require(magrittr)
  require(xts)
  require(PerformanceAnalytics)
  connStr = "driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true"
  cn = odbcDriverConnect(connStr)
  qry = "SELECT 
           v.DateReported
          , CASE v.FundClass_UID
              WHEN 99 THEN 'ALPHA_WAvg'
              WHEN 999 THEN 'ALPHA_FSL'
              ELSE c.Symbol
            END 'Name'
          , v.NAV_Adjusted 'NAV'
        FROM 
          HAMF.POF_NAV AS v 
          LEFT JOIN Hatteras_SecurityDB.dbo.FundClass AS c ON v.FundClass_UID = c.FundClass_UID
        WHERE
          CASE v.FundClass_UID
              WHEN 99 THEN 'ALPHA_WAvg'
              WHEN 999 THEN 'ALPHA_FSL'
              ELSE c.Symbol
            END IN ('ALPHA_FSL', 'ALPHA_WAvg', 'ALPHX', 'ALPIX', 'HHSIX', 'HLSIX', 'HFINX', 'HMFIX')
        ORDER BY
          v.DateReported"
  pofs = dcast.data.table(data.table(sqlQuery(cn,qry)), formula = DateReported ~ Name, fun.aggregate = mean)
  pofs$DateReported = as.Date.factor(pofs$DateReported)
  res = as.xts.data.table(pofs)
  res = CalculateReturns(res)
  
  maxDate = as.Date.character(end(na.omit(res)), format = '%Y-%m-%d')
  qry2 = "SELECT CAST(MAX(d.DateReported) AS date) 'DateReported' FROM Hatteras_SecurityDB.dbo.DailyTotalReturn AS d"
  
  maxDateDTR = sqlQuery(cn,qry2)
  maxDateDTR = as.Date.factor(maxDateDTR[[1]])
  
  if(maxDate!=maxDateDTR)
  {
      qry3 = "SELECT CAST(d.DateReported as date) 'DateReported', c.Symbol, d.OneDayReturn/100 'OneDayReturn' FROM Hatteras_SecurityDB.dbo.DailyTotalReturn AS d LEFT JOIN hatteras_Securitydb.dbo.fundclass AS c ON d.Fund_UID = c.Fund_UID and d.FundClass_UID = c.FundClass_UID WHERE d.DateReported > '"
      qry3 = paste(qry3, maxDate, sep = ' ')
      qry3 = paste(qry3,"') as A",  sep = ' ' )
      qry3 = paste("SELECT p.* FROM (", qry3, sep = '')
      qry3 = paste(qry3, "PIVOT(MAX(a.OneDayReturn) FOR a.SYMBOL IN ([ALPHX],[ALPIX],[HHSIX],[HFINX],[HLSIX], [HMFIX])) AS p ORDER BY p.DateReported", sep = ' ')
      pofs2 = data.table(sqlQuery(cn, qry3))
      pofs2$DateReported = as.Date.factor(pofs2$DateReported)
      res2 = as.xts.data.table(pofs2)
      if(as.Date(maxDateDTR) %in% index(res))
      {
        res[as.Date(maxDateDTR), names(res2)] = res2
      }
      else
      {
        res2$ALPHA_FSL = NA
        res2$ALPHA_WAvg = NA
        res = rbind(res, res2[, names(res)])
      }
    }
  return(res)
}