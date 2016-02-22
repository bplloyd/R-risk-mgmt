nVaR = function(x, p = 0.99, n = NULL){
  x = na.omit(x)
  if (!is.null(n)){
      nr = nrow(x)
      if((!is.null(n)) & n <= nrow(x))   
          return(qnorm(1-p, mean(x[(nr - n + 1):nr]), sd(x[(nr - n + 1):nr])))
      else
          return(NA)
  }else{
    return(qnorm(1-p, mean(x), sd(x)))
  }
}