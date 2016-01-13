loadPOFs = function(returns = TRUE){
  require(RODBC)
  require(magrittr)
  require(xts)
  require(PerformanceAnalytics)
  connStr = "driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true"
  cn = odbcDriverConnect(connStr)
  qry = "SELECT
				p.*
			FROM
			(
				select 
					CAST(n.DateReported AS datetime) 'DateReported'
					, c.Symbol
					, n.NAV_Adjusted
				from 
					hamf.pof_nav AS n
					LEFT JOIN hatteras_Securitydb.dbo.fundclass AS c ON n.Fund_UID = c.Fund_UID and n.FundClass_UID = c.FundClass_UID	
			 where
          n.FundClass_UId <> 99
    ) AS A
			PIVOT(MAX(A.NAV_Adjusted) FOR A.Symbol IN ([ALPHX],[ALPIX],[HHSIX],[HFINX],[HLSIX], [HMFIX])) AS P
			ORDER BY p.DateReported"
  pofs = sqlQuery(cn,qry)
  pofs= as.xts(pofs[,2:7], order.by = as.Date.character(pofs$DateReported))
  if(returns == T){
      pofs = pofs %>% CalculateReturns()
  }
  return(pofs)
}