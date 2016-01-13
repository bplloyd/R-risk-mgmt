library(RODBC)
cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
library(PerformanceAnalytics)
library(xts)
index.sql = "SELECT        
	p.*
FROM            
(
  SELECT        
  v.DateReported
  , v.Ticker
  , v.Daily_Return
  FROM            
  Hatteras_Sandbox_Tools.dbo.v_Index_Returns_Daily AS v 
  WHERE
  v.Ticker IN ('HFRXEH')
) AS a 
  PIVOT (MAX(a.Daily_Return) FOR a.Ticker IN ([HFRXEH])) AS p
  ORDER BY 
  p.DateReported"
index.result = sqlQuery(cn, index.sql)
index.xts = xts(index.result[,2], order.by = as.Date.POSIXct(index.result[,1]))