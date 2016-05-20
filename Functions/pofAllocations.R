pofAllocations = function(id)
{
    #id = 786
    require(RODBC)
    cn = odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
    require(xts)
    require(data.table)
    require(lubridate)
    qry=paste0("SELECT
    p.*
      FROM
    (
    SELECT
    CAST(v.DateReported AS date) 'DateReported'
    , v.Security_Name
    , COALESCE(v.MarketValue, 0) 'MarketValue'
    FROM
    v_FundSecs_FLASHREPORT AS v
    WHERE
    v.Fund_UID = ", id, "
    ) AS A
    PIVOT(MAX(A.MarketValue) FOR A.Security_NAME IN ([HATTERAS EVENT DRIVEN],[HATTERAS MARKET NEUTRAL], [HATTERAS LONG/SHORT EQUITY], [HATTERAS MARKET NEUTRAL FUND], [HATTERAS RELATIVE VALUE], [MANAGED FUTURES], [HATTERAS MANAGED FUTURES - H], [STIC - LIQUID ASSETS PORTFOLIO], [US DOLLARS])) AS P
    ORDER BY
    p.DateReported")
    
    assets= sqlQuery(cn,qry)
    assets = xts(assets[,2:ncol(assets)], order.by = as.Date(assets$DateReported))
    assets$`MANAGED FUTURES` = na.fill(assets$`MANAGED FUTURES`,0) + na.fill(assets$`HATTERAS MANAGED FUTURES - H`,0)
    assets$`HATTERAS MARKET NEUTRAL` = na.fill(assets$`HATTERAS MARKET NEUTRAL FUND`,0) + na.fill(assets$`HATTERAS MARKET NEUTRAL`,0)
    
    assets = na.fill(assets[, c("HATTERAS EVENT DRIVEN", "HATTERAS LONG/SHORT EQUITY", "HATTERAS MARKET NEUTRAL", "HATTERAS RELATIVE VALUE", "MANAGED FUTURES", "STIC - LIQUID ASSETS PORTFOLIO")],0)
    names(assets) = c("ED", "LSE", "MN", "LSD", "MF", "STIC")
    
    res = xts(t(apply(assets, 1, function(x)return(x/sum(x)))), order.by = index(assets))
    index(res) = ymd(index(res))
    return(res)
}   
    
#     weights.alpha = assets.alpha[,1]/assets.alpha.all
#     for(i in 2:ncol(assets.alpha))
#       weights.alpha = cbind(weights.alpha, assets.alpha[,i]/assets.alpha.all)


