exposureSummary2 = function(id, start=NULL, end=NULL)
{
  library(RODBC)
  library(data.table)
  library(dplyr)
  ch = paste("driver={SQL Server}", "server=HAT-SQL-01","database=Hatteras_Sandbox_Tools", "trusted_connection=true", sep = "; ")
  ch = odbcDriverConnect(ch)

                   
	qry = paste0(" 	            
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
                , SUM(s.Exposure) 'Exposure'
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
              HAMF.Summary_Exposure AS s
              LEFT JOIN HAMF.SubAdvisors AS a ON s.SubAdvised_UID = a.SubAdvised_UID 
              LEFT JOIN HAMF.HAMF_TYPES AS t ON s.Security_Type = t.SecurityType
              LEFT JOIN v_LagDate_FLASHREPORT AS ld ON s.DateReported = ld.Cur
            WHERE 
              s.Asset_Type IN ('EQ', 'OP', 'FI', 'DERV')
              AND s.OptionMode = 1
              AND s.Account_ID = ", id)
	
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
                  END
	         ORDER BY	
               CASE
  	              WHEN ((s.Fund_UID = 777) AND (s.DateReported < '2016-06-01')) THEN ld.Lag
  	              ELSE s.DateReported
	             END
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
    	             WHEN ((s.Fund_UID = 777) AND (s.Asset_Type = 'EQ')) THEN 'MF'
    	             ELSE s.Asset_Type
	                END
	             , s.Security_Type
                , s.Long_Short
	             , CASE
    	             WHEN ((s.Fund_UID = 777) AND (s.Asset_Type = 'EQ')) THEN 'MF'
    	             ELSE s.Sector
	                END
	             , s.SP_Rating
	             , s.Country
	             , s.Mkt_Cap
	             , a.Abbreviation
	             , s.MBS_Type
	            
	             , CASE
	             WHEN s.Fund_UID <> 777 THEN t.HamfType
	             ELSE 'MF'
	             END")
  res = sqlQuery(ch, qry)
  res$DateReported = as.Date.factor(res$DateReported)
  return(tbl_dt(res))
}