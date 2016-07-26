rollCoefs = function(Fm.roll){
  require(xts)
 
  ColNames = colnames(coef(Fm.roll[[1]]))
  fundNames = names(Fm.roll[[length(Fm.roll)]]$asset.fit)
  result = lapply(fundNames, FUN = function(f){res = as.xts(t(sapply(Fm.roll, FUN = function(r)return(t(coef(r)[f,])))));   
                                      index(res) = as.Date(index(res))
                                      names(res) = ColNames
                                      return(res)})
  names(result) = fundNames
  return(result)
}