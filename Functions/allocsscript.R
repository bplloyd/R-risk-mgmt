loadSubAdvisors = function(){
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  require(xts)
  require(data.table)
  qry = "SELECT 
	*
FROM
	(
		SELECT
			v.DateReported
			, s.Abbreviation
			, v.NetAssets / u.NetAssets 'Allocation'
		FROM
			v_Assets_SUB_FLASHREPORT AS v
			LEFT JOIN v_Assets_UFT_FLASHREPORT AS u ON (v.DateReported = u.DateReported AND v.Fund_UID = u.Fund_UID)
			LEFT JOIN Hamf.SubAdvisors AS s ON v.SubAdvised_UID = s.SubAdvised_UID
		WHERE
			u.Fund_UID = 785
	) AS A
PIVOT(MAX(A.Allocation) FOR A.Abbreviation IN ([Apis], [BoardmanBay], [BlueJay], [Coe], [ISF], [LoremIpsum], [MiscLSE])) AS p
ORDER BY 
	p.DateReported "
  res = sqlQuery(cn, qry)
  res = xts(res[, 2:8], order.by = as.Date.factor(res$DateReported))
 
}
