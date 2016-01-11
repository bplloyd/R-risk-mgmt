loadPOFs = function(){
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  require(xts)
  pofs = sqlQuery(cn,"SELECT
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
			) AS A
			PIVOT(MAX(A.NAV_Adjusted) FOR A.Symbol IN ([ALPHX],[ALPIX],[HHSIX],[HFINX],[HLSIX], [HMFIX])) AS P
			ORDER BY p.DateReported")
  pofs= as.xts(pofs[,2:7], order.by = as.Date.character(pofs$DateReported))
  return(pofs)
}