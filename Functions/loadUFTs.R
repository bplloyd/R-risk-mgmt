loadUFTs = function(){
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
    
  ufts = dcast.data.table(data.table(sqlQuery(cn,qry)), formula = DateReported ~ Name, fun.aggregate = mean)
  ufts$DateReported = as.Date.factor(ufts$DateReported)
  res = as.xts.data.table(ufts)
  res = CalculateReturns(res)
  return(res)
}