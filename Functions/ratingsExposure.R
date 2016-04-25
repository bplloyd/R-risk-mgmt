ratingsExposure = function(id, on = "weeks")
{
  library(RODBC)
  library(data.table)
  library(xts)
  library(PerformanceAnalytics)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  sql = paste0("SELECT
                	e.DateReported
                  , e.SP_Rating
                  , SUM(COALESCE(e.Exposure,0)) 'Exposure'
              FROM
                  HAMF.Summary_Exposure AS e
              WHERE
                  e.Asset_Type IN ('FI', 'EQ', 'OP', 'DERV', 'FT')
                  AND e.OptionMode = 1
                  AND e.Account_ID = ", id, " GROUP BY
                                            e.DateReported
                                            , e.SP_Rating
                                         ORDER BY
                                            e.DateReported
                                          , e.SP_Rating")
  res = sqlQuery(cn, sql)
 
                 
}