require(RODBC)
cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
require(xts)
require(dplyr)
require(stringr)
require(lubridate)

#id = 59
startDate = mdy('4-29-2016')
endDate = mdy('5-19-2016')

qry = paste0(
  "WITH
  Weights AS
  (
  select 
  ld.Cur 'DateReported'
  , v.DateReported 'Date_Weight'
  , v.Security_Name
  , v.Security_UID
  , v.Asset_Type
  , v.Security_Type
  , COALESCE(
  CASE 
  WHEN v.Asset_Type <> 'OP' THEN v.Ticker
  ELSE od.UnderlyingTicker
  END,
  v.Ticker
  ) 'Ticker'
  , CASE 
  WHEN ((v.Code = 4105) OR (v.Code = 4110)) THEN Hatteras_Sandbox_Tools.dbo.ufn_LongShortAdjustment_OP_FLASHREPORT(v.Code, v.Quantity)							
  WHEN (v.Code = 4420) THEN Hatteras_Sandbox_Tools.dbo.ufn_LongShortAdjustment_CDS_FLASHREPORT(v.Code, v.Security_Name, v.LongShort)				
  ELSE v.LongShort
  END 'Long_Short'
  , COALESCE(CASE 
  WHEN v.Asset_Type <> 'OP' THEN v.Cusip
  ELSE od.Underlying_Cusip
  END,
  v.Cusip
  ) 'Cusip'
  , COALESCE(
  CASE 
  WHEN v.Asset_Type <> 'OP' THEN 
  CASE 
  WHEN v.Code <> 5400 THEN v.Sector
  ELSE 'ETF'
  END
  ELSE 
  CASE 
  WHEN od.Underlying_Code <> 5400 THEN od.Underlying_Sector 
  ELSE 'ETF'
  END
  END,
  v.Sector
  ) 'Sector'
  , CAST(
  (
  
  CASE 
  WHEN ((v.Code = 4105 OR v.Code = 4110) AND od.OptDelta IS NOT NULL) THEN Hatteras_Sandbox_Tools.dbo.ufn_GetDeltaAdjustedExposure_FLASHREPORT(od.OptDelta, v.Quantity, od.Underlying_Price, 100)/v.TotalNetAssets
  WHEN (v.Code = 4420)  THEN Hatteras_Sandbox_Tools.dbo.ufn_CDSExposureAdjustment_FLASHREPORT(v.Code, v.Security_Name, v.Quantity, v.Price, v.MarketValue)/v.TotalNetAssets															
  ELSE v.MarketValue/v.TotalNetAssets
  END
  
  )
  AS decimal(23,15)
  ) 'Exposure'
  , v.BloombergID
  from 
  v_SubSecs_FLASHREPORT AS v 
  LEFT JOIN v_LagDate_FLASHREPORT AS ld ON v.DateReported = ld.Lag
  LEFT JOIN Hatteras_Sandbox_Tools.dbo.v_Options_Underlying_FLASHREPORT As od ON ((v.DateReported = od.DateReported) AND (v.Security_UID = od.Option_Security_UID) AND (v.Asset_Type = 'OP'))
  LEFT JOIN Hatteras_Sandbox_Tools.dbo.v_DateMatchBB AS dm ON v.DateReported = dm.DateReported
  LEFT JOIN Hatteras_SecurityDB.dbo.BloombergSecuritiesUpdate AS bb ON ((dm.RunDate = bb.RunDate) AND (v.Security_UID = bb.Security_UID) AND (v.Code <> 2700))
  WHERE 
  ld.Cur BETWEEN '", startDate, "' AND '", endDate, "'
  ),
  Returns AS
  (
  select
  v.DateReported 'Date_Return'
  , v.Security_Name
  , v.Security_UID
  , v.Asset_Type
  , v.Security_Type
  , COALESCE(CASE 
  WHEN v.Asset_Type <> 'OP' THEN v.Ticker
  ELSE od.UnderlyingTicker
  END,
  v.Ticker
  ) 'Ticker'
  , CASE 
  WHEN ((v.Code = 4105) OR (v.Code = 4110)) THEN Hatteras_Sandbox_Tools.dbo.ufn_LongShortAdjustment_OP_FLASHREPORT(v.Code, v.Quantity)							
  WHEN (v.Code = 4420) THEN Hatteras_Sandbox_Tools.dbo.ufn_LongShortAdjustment_CDS_FLASHREPORT(v.Code, v.Security_Name, v.LongShort)				
  ELSE v.LongShort
  END 'Long_Short'
  , COALESCE(CASE 
  WHEN v.Asset_Type <> 'OP' THEN v.Cusip
  ELSE od.Underlying_Cusip
  END,
  v.Cusip
  ) 'Cusip'
  , COALESCE(
  CASE 
  WHEN v.Asset_Type <> 'OP' THEN 
  CASE 
  WHEN v.Code <> 5400 THEN v.Sector
  ELSE 'ETF'
  END
  ELSE 
  CASE 
  WHEN od.Underlying_Code <> 5400 THEN od.Underlying_Sector 
  ELSE 'ETF'
  END
  END,
  v.Sector
  ) 'Sector'
  , v.PercPriceChange
  , v.BloombergID
  , v.NavImpact
  from 
  v_SubSecs_FLASHREPORT AS v 
  LEFT JOIN Hatteras_Sandbox_Tools.dbo.v_Options_Underlying_FLASHREPORT As od ON ((v.DateReported = od.DateReported) AND (v.Security_UID = od.Option_Security_UID) AND (v.Asset_Type = 'OP'))
  LEFT JOIN Hatteras_Sandbox_Tools.dbo.v_DateMatchBB AS dm ON v.DateReported = dm.DateReported
  LEFT JOIN Hatteras_SecurityDB.dbo.BloombergSecuritiesUpdate AS bb ON ((dm.RunDate = bb.RunDate) AND (v.Security_UID = bb.Security_UID) AND (v.Code <> 2700))
  WHERE 
  v.DateReported BETWEEN '", startDate, "' AND '", endDate, "'
  )
  SELECT
  CAST(w.Date_Weight AS date) 'Date_Weight'
  , CAST(r.Date_Return AS date) 'Date_Return'
  , COALESCE(r.Security_Name, w.Security_Name) 'Security_Name'
  , COALESCE(r.Ticker, w.Ticker) 'Ticker'
  , COALESCE(r.Cusip,w.Cusip) 'Cusip'
  , COALESCE(r.Asset_Type,w.Asset_Type) 'Asset_Type'
  , COALESCE(r.Security_Type,w.Security_Type) 'Security_Type'
  , COALESCE(r.Long_Short, w.Long_Short) 'LongShort'
  , COALESCE(r.BloombergID,w.BloombergID) 'BloombergID'
  , COALESCE(r.Sector, w.Sector) 'Sector'
  , r.PercPriceChange 'Return'
  , w.Exposure
  , r.NavImpact
  FROM
  Returns AS r
  LEFT JOIN v_LagDate_FLASHREPORT AS ld ON r.Date_Return= ld.Cur
  FULL OUTER JOIN Weights AS w ON (ld.Lag = w.Date_Weight AND r.SubAdvised_UID = w.SubAdvised_UID AND r.Security_UID = w.Security_UID AND r.Long_Short = w.Long_Short)
  WHERE
  r.Date_Return > '", startDate, "'
  AND COALESCE(r.Asset_Type,w.Asset_Type) NOT IN ('ST', 'CASH')
  ORDER BY r.Date_Return, COALESCE(r.Security_Name, w.Security_Name)"
)
res = sqlQuery(cn,qry)