
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  require(xts)
  require(data.table)
  qry = "select v.DateReported, v.NetAssets from v_Assets_UFT_FLASHREPORT AS v WHERE v.Fund_UID = 784 ORDER BY v.DateReported"
  assets = sqlQuery(cn,qry)

  assets = xts(assets$NetAssets, order.by = as.Date.factor(assets$DateReported))
  lsd = cbind(ufts$LSD['/201603'], assets['/201603'])
  

