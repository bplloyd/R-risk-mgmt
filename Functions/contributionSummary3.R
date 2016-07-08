contributionSummary3 = function(id, start=NULL, end=NULL)
{
  library(RODBC)
  library(data.table)
  library(dplyr)
  library(stringi)
  ch = paste("driver={SQL Server}", "server=HAT-SQL-01","database=Hatteras_Sandbox_Tools", "trusted_connection=true", sep = "; ")
  ch = odbcDriverConnect(ch)

  if(id < 300)
  {
    navSource = "HAMF.SUB_NAV"
    idCol = "SubAdvised_UID"
  }  
  if(id %in% c(777, 782, 783, 784, 785))
  {
    navSource = "HAMF.UFT_NAV"
    idCol = "Fund_UID"
  }
  if(id %in% c(774, 786))
  {
    navSource = "dbo.v_NAV_POF_Calculated_FLASHREPORT"
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
                    		", navSource, " AS n
                    		LEFT JOIN v_LagDate_FLASHREPORT AS ld ON n.DateReported = ld.Lag
                    	WHERE
                    		n.", idCol, " = ", id)
  
  if(!is.null(start))
  {
    qry = paste0(qry, " AND ld.Cur >= '", start, "'")
  }
  if(!is.null(end))
  {
    qry = paste0(qry, " AND ld.Cur <= '", end, "'")
  }
                   
	qry = paste0(qry, ") 	              
              SELECT	
                CASE
                    WHEN ((s.Fund_UID = 777) AND (s.DateReported < '2016-06-01')) THEN ld.Lag
                    ELSE s.DateReported
                END 'DateReported'
                , CASE
                      WHEN ((s.Fund_UID = 777) AND (s.Asset_Type = 'EQ')) THEN 'MF'
                      ELSE s.Asset_Type
                  END 'Asset_Type'
                , s.Security_Type
                 , CASE
                      WHEN ((s.Fund_UID = 777) AND (s.Asset_Type = 'EQ')) THEN 'MF'
                      ELSE s.Sector
                    END 'Sector'
                , s.SP_Rating
                , s.Country
                , s.Long_Short
                , s.Mkt_Cap
                , SUM(s.NAV_Contribution_Total/n.NAV) 'Contribution_Total'
                , SUM(s.NAV_Contribution_Market/n.NAV) 'Contribution_Market'
                , SUM(s.NAV_Contribution_Income/n.NAV) 'Contribution_Income'
                , SUM(s.NAV_Contribution_Expenses/n.NAV) 'Contribution_Expenses'
                , a.Abbreviation 'SubAdvisor'
                , s.MBS_Type
                , CASE s.Fund_UID
                      WHEN 777 THEN 'MF'
                      WHEN 782 THEN 'MN'
                      WHEN 783 THEN 'ED'
                      WHEN 784 THEN 'LSD'
                      WHEN 785 THEN 'LSE'
                      WHEN 786 THEN 'ALPHA'
                      WHEN 774 THEN 'HHSIX'
                  END 'Fund'
                , s.SubAdvised_UID
                , CASE
                    WHEN s.Fund_UID <> 777 THEN t.HamfType
                    ELSE 'MF'
                  END 
                  'HAMF_TYPE'
            FROM 
              HAMF.Summary_Contribution2 AS s
              LEFT JOIN StartNavs AS n ON (n.DateReported = s.DateReported AND n.Id = s.Account_ID)
              LEFT JOIN HAMF.SubAdvisors AS a ON s.SubAdvised_UID = a.SubAdvised_UID 
              LEFT JOIN HAMF.HAMF_TYPES AS t ON s.Security_Type = t.SecurityType
              LEFT JOIN v_LagDate_FLASHREPORT AS ld ON s.DateReported = ld.Cur
            WHERE 
              s.Account_ID = ", id)
	if(!is.null(start))
	{
	  qry = paste0(qry, " AND  CASE
                    WHEN ((s.Fund_UID = 777) AND (s.DateReported < '2016-06-01')) THEN ld.Lag
                    ELSE s.DateReported
                END >= '", start, "'")
	}
	if(!is.null(end))
	{
	  qry = paste0(qry, " AND  CASE
                    WHEN ((s.Fund_UID = 777) AND (s.DateReported < '2016-06-01')) THEN ld.Lag
                    ELSE s.DateReported
                END <= '", end, "'")
	}
	
	qry = paste0(qry, "
              AND s.OptionMode = 1
            GROUP BY	
               CASE
                    WHEN ((s.Fund_UID = 777) AND (s.DateReported < '2016-06-01')) THEN ld.Lag
               ELSE s.DateReported
               END
              , CASE
                      WHEN ((s.Fund_UID = 777) AND (s.Asset_Type = 'EQ')) THEN 'MF'
               ELSE s.Asset_Type
               END
              , s.Security_Type
             , CASE
                      WHEN ((s.Fund_UID = 777) AND (s.Asset_Type = 'EQ')) THEN 'MF'
                      ELSE s.Sector
                    END
              , s.SP_Rating
              , s.Country
              , s.Long_Short
              , s.Mkt_Cap
              , a.Abbreviation
              , s.MBS_Type
              , CASE s.Fund_UID
                      WHEN 777 THEN 'MF'
                       WHEN 782 THEN 'MN'
                       WHEN 783 THEN 'ED'
                       WHEN 784 THEN 'LSD'
                       WHEN 785 THEN 'LSE'
                       WHEN 786 THEN 'ALPHA'
                       WHEN 774 THEN 'HHSIX'
                      END
              , s.SubAdvised_UID
              , CASE
                    WHEN s.Fund_UID <> 777 THEN t.HamfType
                    ELSE 'MF'
                  END")
  res = sqlQuery(ch, qry)
  res$DateReported = as.Date.factor(res$DateReported)
  res$SP_Rating = as.character.factor(res$SP_Rating)
#   res$SP_Rating[str_detect(res$SP_Rating, "(P)")] = str_sub(res$SP_Rating[str_detect(res$SP_Rating, "(P)")], 4)
#   res$SP_Rating = str_replace(res$SP_Rating, "u", "")
  res$SP_Rating = stri_replace_all_fixed(res$SP_Rating, '(P)', '')
  res$SP_Rating = stri_replace_all_fixed(res$SP_Rating, 'u', '')
  res$SP_Rating = stri_replace_all_fixed(res$SP_Rating, '+', '_p')
  res$SP_Rating = stri_replace_all_fixed(res$SP_Rating, '-', '_m')
  res$SP_Rating = as.factor(res$SP_Rating)
  return(tbl_dt(res))
}