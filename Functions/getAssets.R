
  require(RODBC)
  cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  require(xts)
  require(data.table)
  qry.uft = "select v.DateReported, v.NetAssets from v_Assets_UFT_FLASHREPORT AS v WHERE v.Fund_UID = 777 ORDER BY v.DateReported"
  assets.uft = sqlQuery(cn,qry.uft)
  assets.uft = xts(assets.uft$NetAssets, order.by = as.Date(assets.uft$DateReported))
  names(assets.uft) = "MF"
  
  
  qry.row = "select v.DateReported, v.NetAssets from v_Assets_Sub_FLASHREPORT AS v WHERE v.SubAdvised_UID = 105 ORDER BY v.DateReported"
  assets.row = sqlQuery(cn,qry.row)
  assets.row = xts(assets.row$NetAssets, order.by = as.Date(assets.row$DateReported))
  names(assets.row) = "Row"
  
  qry.rev = "select v.DateReported, v.NetAssets from v_Assets_Sub_FLASHREPORT AS v WHERE v.SubAdvised_UID = 34 ORDER BY v.DateReported"
  assets.rev = sqlQuery(cn,qry.rev)
  assets.rev = xts(assets.rev$NetAssets, order.by = as.Date(assets.rev$DateReported))
  names(assets.rev) = "Revolution"
  
  qry.misc = "select v.DateReported, v.NetAssets from v_Assets_Sub_FLASHREPORT AS v WHERE v.SubAdvised_UID = 32 ORDER BY v.DateReported"
  assets.misc = sqlQuery(cn,qry.misc)
  assets.misc= xts(assets.misc$NetAssets, order.by = as.Date(assets.misc$DateReported))
  names(assets.misc) = "Misc"

  weights2 = cbind(assets.misc/(assets.misc + assets.rev + assets.row), assets.rev/(assets.misc + assets.rev + assets.row), assets.row/(assets.misc + assets.rev + assets.row))
  
  mf.cont = weights.april$MiscMF*mf.april$MiscMF
  mf.cont = cbind(mf.cont, weights.april$Revolution*mf.april$Revolution)
  mf.cont = cbind(mf.cont, weights.april$Row*mf.april$Row)

  mf.cont2 = weights.april2$MiscMF*mf.april$MiscMF
  mf.cont2 = cbind(mf.cont2, weights.april2$Revolution*mf.april$Revolution)
  mf.cont2 = cbind(mf.cont2, weights.april2$Row*mf.april$Row)
  
  qry.AllSubs = "select 
		v.DateReported
		, s.Abbreviation 'Name'
		, v.NetAssets 
	from 
		v_Assets_Sub_FLASHREPORT AS v 
		LEFT JOIN HAMF.SubAdvisors AS s ON v.SubAdvised_UID = s.SubAdvised_UID
		ORDER BY v.DateReported"
  assets.AllSubs = sqlQuery(cn,qry.AllSubs)
  assets.AllSubs = dcast.data.table(data.table(assets.AllSubs), formula = DateReported ~ Name, fun.aggregate = mean)
  assets.AllSubs$DateReported = as.Date.factor(assets.AllSubs$DateReported)
  assets.AllSubs = as.xts.data.table(assets.AllSubs)
  assets.ED = assets.AllSubs[, c("Apis", "BoardmanBay", "Coe", "ISF", "MiscLSE")]
  assets.MN = assets.AllSubs[, c("Jadwin", "Longbow", "MiscMN")]
  assets.MF = assets.AllSubs[, c("Revolution", "Row", "MiscMF")]
  assets.ED = assets.AllSubs[, c("FrontFour","Havens", "MiscED")]
  assets.LSD = assets.AllSubs[, c("MeehanCombs", "MatlinPatterson", "SmithBreeden", "Soundpoint", "MiscLSD")]

  
  weights.LSE = assets.LSE[,1]/assets.ufts$LSE
  weights.LSE.alpha = weights.LSE[,1]*weights.alpha$LSE
  cont.alpha.LSE = lag(weights.LSE.alpha[,1])*subs[, names(weights.LSE.alpha)[1]]
  
  for(i in 2:ncol(assets.LSE))
  {
      weights.LSE = cbind(weights.LSE, assets.LSE[,i]/assets.ufts$LSE )
      weights.LSE.alpha = cbind(weights.LSE.alpha, weights.LSE[,i]*weights.alpha$LSE)
      cont.alpha.LSE = cbind(cont.alpha.LSE, lag(weights.LSE.alpha[,i])*subs[, names(weights.LSE.alpha)[i]])
  }
 
  
  
  weights.LSD = assets.LSD[,1]/assets.ufts$LSD
  weights.LSD.alpha = weights.LSD[,1]*weights.alpha$LSD
  cont.alpha.LSD = lag(weights.LSD.alpha[,1])*subs[, names(weights.LSD.alpha)[1]]
  
  for(i in 2:ncol(assets.LSD))
  {
    weights.LSD = cbind(weights.LSD, assets.LSD[,i]/assets.ufts$LSD )
    weights.LSD.alpha = cbind(weights.LSD.alpha, weights.LSD[,i]*weights.alpha$LSD)
    cont.alpha.LSD = cbind(cont.alpha.LSD, lag(weights.LSD.alpha[,i])*subs[, names(weights.LSD.alpha)[i]])
  }
  
  
  weights.ED = assets.ED[,1]/assets.ufts$ED
  weights.ED.alpha = weights.ED[,1]*weights.alpha$ED
  cont.alpha.ED = lag(weights.ED.alpha[,1])*subs[, names(weights.ED.alpha)[1]]
  
  for(i in 2:ncol(assets.ED))
  {
    weights.ED = cbind(weights.ED, assets.ED[,i]/assets.ufts$ED )
    weights.ED.alpha = cbind(weights.ED.alpha, weights.ED[,i]*weights.alpha$ED)
    cont.alpha.ED = cbind(cont.alpha.ED, lag(weights.ED.alpha[,i])*subs[, names(weights.ED.alpha)[i]])
  }
  
  
  weights.MN = assets.MN[,1]/assets.ufts$MN
  weights.MN.alpha = weights.MN[,1]*weights.alpha$MN
  cont.alpha.MN = lag(weights.MN.alpha[,1])*subs[, names(weights.MN.alpha)[1]]
  
  for(i in 2:ncol(assets.MN))
  {
    weights.MN = cbind(weights.MN, assets.MN[,i]/assets.ufts$MN )
    weights.MN.alpha = cbind(weights.MN.alpha, weights.MN[,i]*weights.alpha$MN)
    cont.alpha.MN = cbind(cont.alpha.MN, lag(weights.MN.alpha[,i])*subs[, names(weights.MN.alpha)[i]])
  }
  
  weights.MF = assets.MF[,1]/assets.ufts$MF
  weights.MF.alpha = weights.MF[,1]*weights.alpha$MF
  cont.alpha.MF = lag(weights.MF.alpha[,1])*subs[, names(weights.MF.alpha)[1]]
  
  for(i in 2:ncol(assets.MF))
  {
    weights.MF = cbind(weights.MF, assets.MF[,i]/assets.ufts$MF )
    weights.MF.alpha = cbind(weights.MF.alpha, weights.MF[,i]*weights.alpha$MF)
    cont.alpha.MF = cbind(cont.alpha.MF, lag(weights.MF.alpha[,i])*subs[, names(weights.MF.alpha)[i]])
  }
  
  cont.alpha = cbind(cont.alpha.ED, cont.alpha.LSE, cont.alpha.LSD, cont.alpha.MN, cont.alpha.MF)
  cont.alpha = cont.alpha['201604']
  
  