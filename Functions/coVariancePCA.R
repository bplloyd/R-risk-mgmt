coVariancePCA = function(cov.mat, R=NULL, na.mode = "zero")
{
  require(xts)
  eig = eigen(cov.mat)
  pca = list(StdDevs = sqrt(eig$values),
             Components = eig$vectors,
             ComponentSeries = NULL)
  colnames(pca$Components) = paste0("PC", 1:ncol(cov.mat))
  row.names(pca$Components) = colnames(cov.mat)
  
  if(!is.null(R))
  {
    componentSeries = as.xts(
                              switch(na.mode,
                                     zero = apply(X = R, 
                                                  MARGIN = 1, 
                                                  FUN = function(r)return(na.fill(r, fill = 0) %*% pca$Components[,1])
                                                  ),
                                     omit = apply(X = na.omit(R),
                                                  MARGIN = 1,
                                                  FUN = function(r)return(r %*% pca$Components[,1])
                                                  )
                                    )
                            )
    for(i in 2:ncol(R))
    {
      componentSeries = cbind(componentSeries, 
                              as.xts(
                                switch(na.mode,
                                       zero = apply(X = R, 
                                                    MARGIN = 1, 
                                                    FUN = function(r)return(na.fill(r, fill = 0) %*% pca$Components[,i])
                                                    ),
                                       omit = apply(X = na.omit(R),
                                                    MARGIN = 1,
                                                    FUN = function(r)return(r %*% pca$Components[,i])
                                                    )
                                       )
                                    )
                              )
    }
                                               
    
    index(componentSeries) = as.Date(index(componentSeries))
    names(componentSeries) = colnames(pca$Components)
    
    pca$ComponentSeries = componentSeries
  }
  return(pca)
}