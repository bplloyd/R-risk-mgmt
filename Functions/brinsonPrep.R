require(RODBC)
cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
require(xts)
require(dplyr)
require(stringr)
require(lubridate)

id = 59
startDate = mdy('5-13-2016')
endDate = mdy('5-18-2016')

qry = paste0("WITH
        Weights AS
             (
             select 
             ld.Cur 'DateReported'
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
             v.SubAdvised_UID = ", id, "
             AND v.DateReported BETWEEN '", startDate, "' AND '", endDate, "'
             ),
             Returns AS
  (
             select
             v.DateReported
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
             v.SubAdvised_UID = ", id, "
             AND v.DateReported BETWEEN '", startDate, "' AND '", endDate, "'
             )
             SELECT
	  CAST(COALESCE(r.DateReported, w.DateReported) AS date) 'DateReported'
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
  LEFT JOIN v_LagDate_FLASHREPORT AS ld ON r.DateReported = ld.Cur
  FULL OUTER JOIN Weights AS w ON (ld.Lag = w.DateReported AND wr.Security_UID = w.Security_UID AND r.Long_Short = w.Long_Short)
  WHERE
    COALESCE(r.Asset_Type,w.Asset_Type) NOT IN ('ST', 'CASH')
  ORDER BY COALESCE(r.Security_Name, w.Security_Name)
             ")
res = sqlQuery(cn,qry)
load(file = "G:\\PORTFOLIO MANGEMENT\\Bryan Lloyd\\2016 Projects\\Data\\SP500 RData\\20160518.RData")
data_0518 = data

for(i in 1:ncol(data)){
  if(is.factor(data[,i])){
    data[,i] = as.character.factor(data[,i])
  }
}
for(i in 1:ncol(res)){
  if(is.factor(res[,i])){
    res[,i] = as.character.factor(res[,i])
  }
}
colnames(data)[which(colnames(data)=="member")] = "Ticker"
colnames(data)[which(colnames(data)=="ID_CUSIP")] = "Cusip"
colnames(data)[which(colnames(data)=="ID_SEDOL1")] = "Sedol"
names(data)[2:3] = c("Weights_start", "Weights_end")

data$Ticker = str_trim(str_sub(data$Ticker, start = 1, end = str_locate(data$Ticker, " ")[,1]))

myPort = data.frame(Ticker = sort(unique(c(res$Ticker, data$Ticker))), stringsAsFactors = F)
myPort$Weight_BM = na.fill(merge(x=myPort, y=data, by = c("Ticker"), all =T)$Weights_start, fill = 0)/100
myPort$Weight_Port = na.fill(merge(x=myPort, y=res, by = c("Ticker"), all =T)$Exposure, fill = 0)

bb_sectors =  merge(x=myPort, y=data, by = c("Ticker"), all =T)$BICS_LEVEL_1_SECTOR_NAME
hamf_sectors= merge(x=myPort, y = res, by = c("Ticker"), all = T)$Sector
hamf_sectors[which(hamf_sectors=="Telecommunication Services")] = "Communications"
hamf_sectors[which(hamf_sectors=="Information Technology")] = "Technology"

myPort$Sector = ifelse(is.na(bb_sectors), hamf_sectors, bb_sectors)

bb_rets = merge(x=myPort, y = data, by = c("Ticker"), all = T)$CUST_TRR_RETURN_HOLDING_PER/100
hamf_rets = merge(x=myPort, y = res, by = c("Ticker"), all = T)$Return
myPort$Return = ifelse(is.na(bb_rets), hamf_rets, bb_rets)

myPort$Date = rep(endDate, nrow(myPort))
myPort$Sector = as.factor(myPort$Sector)
#myPort = myPort[-which(apply(myPort[,c("Sector", "Return")], 1, function(x)return(sum(is.na(x))))==2),]
brin = brinson(x=myPort, date.var = "Date", cat.var = "Sector", bench.weight = "Weight_BM", portfolio.weight = "Weight_Port", ret.var = "Return")

                        