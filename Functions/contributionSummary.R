contributionSummary = function(id, start, end)
{
  library(RODBC)
  library(data.table)
  library(dplyr)
  ch = paste("driver={SQL Server}", "server=HAT-SQL-01","database=Hatteras_Sandbox_Tools", "trusted_connection=true", sep = "; ")
  ch = odbcDriverConnect(ch)
#   id = 58
#   start = "2016-03-01"
#   end = "2016-03-31"
#   proc = "usp_Summary_Contribution_WAREHOUSE_TESTVERSION"
#   p.id = paste("@id", id, sep = "=")
  start = paste0("'", start, "'")
#   p.start = paste("@startDate", start, sep = "=")
#   if(!is.null(end))
#   {
    end = paste0("'", end, "'")
#     p.end = paste("@endDate", end, sep = "=")
#   }
#   else
#   {
#     p.end = paste("@endDate", start, sep = "=") 
#   }
#   params = paste(p.id, p.start, p.end, sep = ",")
#   #executeSP(proc, params)
#   return(executeSP(proc, params))
  if(id < 300)
  {
    navSource = "SUB_NAV"
    idCol = "SubAdvised_UID"
  }  
  if(id %in% c(777, 782, 783, 784, 785))
  {
    navSource = "UFT_NAV"
    idCol = "Fund_UID"
  }
    
  qry = paste0("WITH 
                  StartNavs AS 
                  (
                    	SELECT
                    		ld.Cur 'DateReported'
                    		, n.NAV
                    		, n.", idCol, " 'Id'
                    	FROM 
                    		HAMF.", navSource, " AS n
                    		LEFT JOIN v_LagDate_FLASHREPORT AS ld ON n.DateReported = ld.Lag
                    	WHERE
                    		n.", idCol, " = ", id, "
                    		AND ld.Cur BETWEEN ", start, " AND ", end, "
		              )
              SELECT	
                s.DateReported
                , s.Asset_Type
                , s.Security_Type
                , s.Sector
                , s.SP_Rating
                , s.Country
                , s.Long_Short
                , s.Mkt_Cap
                , SUM(s.NAV_Contribution_Total/n.NAV) 'Contribution_Total'
                , SUM(s.NAV_Contribution_Market/n.NAV) 'Contribution_Market'
                , SUM(s.NAV_Contribution_Income/n.NAV) 'Contribution_Income'
                , SUM(s.NAV_Contribution_Expenses/n.NAV) 'Contribution_Expenses'
                , s.SubAdvisor
                , s.MBS_Type
                , s.Fund_UID
                , s.SubAdvised_UID
            FROM 
              HAMF.Summary_Contribution AS s
              LEFT JOIN StartNavs AS n ON (n.DateReported = s.DateReported AND n.Id = s.Account_ID)
            WHERE 
              s.Account_ID = ", id, "
              AND s.datereported BETWEEN ", start, " AND ", end, "
              AND s.OptionMode = 1
            GROUP BY	
              s.DateReported
              , s.Asset_Type
              , s.Security_Type
              , s.Sector
              , s.SP_Rating
              , s.Country
              , s.Long_Short
              , s.Mkt_Cap
              , s.SubAdvisor
              , s.MBS_Type
              , s.Fund_UID
              , s.SubAdvised_UID")
  res = sqlQuery(ch, qry)
  res$DateReported = as.Date.factor(res$DateReported)
  #res = as.data.table(res)
  return(tbl_dt(res))
}