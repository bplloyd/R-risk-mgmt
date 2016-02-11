extractAvgCorrelation = function(cor.roll, includeMisc = F)
{
    require(xts)
    require(stockPortfolio)
    result = xts(rep(NA, length(cor.roll)), order.by = as.Date.character(names(cor.roll)))
    for(i in 1:length(cor.roll))
    {
        m = cor.roll[[i]]
        if(!includeMisc){
            m = m[-grep('Misc', rownames(m)), -grep('Misc', colnames(m))]
        }
        if(any(is.na(m))){
            m = m[-which(rowSums(is.na(m))==ncol(m)), -which(rowSums(is.na(m))==ncol(m))]
        }
        if(!is.null(ncol(m))){
            result[names(cor.roll)[i]] = getCorr(m)
        }
    }
    return(result)
}