patchMF = function(ufts)
{
  require(RODBC)
  require(magrittr)
  require(xts)
  require(PerformanceAnalytics)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  
  patch = sqlQuery(cn,"select
	                  d.DateReported
                   , d.OneDayReturn/100 'DailyReturn'
                   FROM
                   Hatteras_SecurityDB.dbo.DailyTotalReturn AS d
                   WHERE
                   d.Fund_UID = 777
                   AND d.SubAdvised_UID IS NULL
                   AND d.DateReported BETWEEN '5-2-2016' AND '5-6-2016'
                   ORDER BY
                   d.DateReported")
  
  patch = xts(patch$DailyReturn, order.by = as.Date(patch$DateReported))
  ufts$MF[index(patch)] = patch
  return(ufts)
}