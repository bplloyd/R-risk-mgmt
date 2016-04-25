qry = "WITH 
	Levels AS
(
  select 
  v.DateReported
  , s.Abbreviation 'Name'
  , COALESCE(l.Liquidity, 99) 'Level'
  , COALESCE(v.MarketValue,0)/a.NetAssets 'Exposure'
  from 
  v_FundSecs_FLASHREPORT AS v
  LEFT JOIN v_Assets_UFT AS a ON (v.Fund_UID = a.Fund_UID AND v.DateReported = a.DateReported)
  LEFT JOIN HAMF.Liquidity AS l ON ((v.SubAdvised_UID = l.SubAdvised_UID) AND (v.Security_UID = l.Security_UID) AND (l.DateReported = (SELECT MAX(l2.DateREported) FROM Hamf.Liquidity AS l2 WHERE l2.DateReported <= v.DateReported)))
  LEFT JOIN HAMF.SubAdvisors AS s ON v.SubAdvised_UID = s.SubAdvised_UID
  WHERE 
  v.DateReported >= '1-22-2016'
  AND v.Fund_UID = 784
  AND v.Asset_Type IN  ('EQ', 'DERV', 'OP', 'FI')
  AND l.Liquidity >= 3
)
select 
l.DateReported
, l.Name
, l.[Level]
, SUM(COALESCE(l.Exposure,0)) 'Exposure'
from 
Levels  as l
GROUP BY
l.DateReported
, l.Name
, l.[Level]
ORDER BY
l.DateReported
, l.Name
, l.[Level] DESC"

require(RODBC)
cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
require(xts)
require(data.table)

lsd.liq = data.table(sqlQuery(cn,qry))
lsd.liq$DateReported = as.Date.factor(lsd.liq$DateReported)
lsd.liq_xts = as.xts(dcast.data.table(lsd.liq[, sum(Exposure), by = c("DateReported", "Name")], formula = DateReported ~ Name, value.var = "V1", fun.aggregate = sum))
