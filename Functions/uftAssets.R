uftAssets = function()
{
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  require(xts)
  require(data.table)
  
  qry.ufts = "SELECT
  p.DateReported
  , p.[777] 'MF'
  , p.[782] 'MN'
  , p.[783] 'ED'
  , p.[784] 'LSD'
  , p.[785] 'LSE'
  FROM
  (
  SELECT
  v.DateReported
  , v.Fund_UID
  , v.NetAssets
  FROM
  v_Assets_UFT_FLASHREPORT AS v
  WHERE (1=1)
  --AND v.DateReported BETWEEN '3-31-2016' AND '4-29-2016'
  --ORDER BY
  --	v.DateReported
  --	, v.Security_NAME
  ) AS A
  PIVOT(MAX(A.NetAssets) FOR A.Fund_UID IN ([777], [782], [783], [784], [785])) AS P
  ORDER BY
  p.DateReported"
  
  assets.ufts= sqlQuery(cn,qry.ufts)
  assets.ufts = xts(assets.ufts[,2:ncol(assets.ufts)], order.by = as.Date(assets.ufts$DateReported))
  return(assets.ufts)
}