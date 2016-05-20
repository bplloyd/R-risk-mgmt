getPositionData = function(ticks)
{
  require(quantmod)
  require(RODBC)
  
  out = vector("list", length(ticks))
  names(out) = ticks
               
              
  for(i in 1:length(ticks)){
    out[[i]] = try(getSymbols(Symbols = ticks[i], auto.assign = F))
  }
  
  nlls = names(out)[which(sapply(out, function(o)return(is.null(nrow(o[1])))))]
  if(length(nlls)>0){
      require(RODBC)
      cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
      for(i in 1:length(nlls))
      {
          qry = paste0(
            "SELECT DISTINCT
                ld.Cur 'DateReported'
                , v.PercPriceChange
              FROM
                  v_LagDate_FLASHREPORT AS ld
                  LEFT JOIN v_FundSecs_FLASHREPORT AS v ON ld.Cur = v.DateReported
              WHERE
                v.Ticker = '", nlls[i], "'
              ORDER BY
                ld.Cur"
          )
          res = sqlQuery(cn, qry)
          out[nlls[i]][[1]] = res
      }
  
  }