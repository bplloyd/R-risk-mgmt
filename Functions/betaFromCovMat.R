betaFromCovMat = function(cov.mat, bm.name)
{
  require(xts)
  
  if(is.xts(cov.mat))
  {
    require(stringr)
    
    varCols = which(lapply(str_split(names(cov.mat), "_"), function(l)return(l[1]==l[2])) == T)
    R.vol = sqrt(cov.mat[,varCols])
    names(R.vol) =  sapply(str_split(names(R.vol),"_"),function(l)return(l[1])) 
    
    R.beta = xts(t(apply(cov.mat[,paste0(names(R.vol), "_", bm.name)], 
                         1, 
                         function(r)
                           return(r/r[which(paste0(names(R.vol), "_", bm.name) == paste0(bm.name, "_", bm.name))])
    )
    )
    , order.by = index(cov.mat)
    )
  }
  else
  {
    R.vol = sapply(row.names(cov.mat)[-which(row.names(cov.mat)==bm.name)], function(n)return(sqrt(cov.mat[n,n])))
    R.beta = sapply(row.names(cov.mat)[-which(row.names(cov.mat)==bm.name)], function(n)return(cov.mat[n, bm.name]/cov.mat[bm.name, bm.name]))
  }
  return(R.beta)
}
  