sectorExposure2 = function(id, filts = NULL, on = "days")
{
  library(RODBC)
  library(data.table)
  library(xts)
  library(PerformanceAnalytics)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  sql = paste0("SELECT p.* FROM
                            (
                              SELECT
                                  e.DateReported
                                  , e.Sector
                                  , SUM(e.Exposure) 'Exposure'
                              FROM
                                  HAMF.Summary_Exposure AS e
                              WHERE 
                                  e.Account_ID =", id)
  if(!is.null(filt))
  {
    filts = paste0("'", filts, "'")
    sql = paste0(sql, " AND e.Asset_Type = ", filt)
  }
  sql = paste0(sql,  " AND e.OptionMode = 1
                      AND e.Asset_Type NOT IN ('ST', 'CASH')
                     GROUP BY
                       e.DateReported
                                  , e.Sector
                              ) AS A
                            PIVOT (MAX(A.Exposure) FOR A.Sector IN ([Consumer Discretionary], [Consumer Staples], 
                                                                    [Energy], [Financials], [Health Care], 
                                                                    [Information Technology], [Materials], 
                                                                    [Industrials], [Utilities], 
                                                                    [Telecommunication Services], [No GIC]
                                                                    )) AS P
                ORDER BY
                    p.DateReported", sep = " ")
  res = sqlQuery(cn, sql)
  res = na.fill(xts(res[,2:ncol(res)], order.by = as.Date.factor(res$DateReported)), fill=0)
  return(res[endpoints(res, on = on),])
}