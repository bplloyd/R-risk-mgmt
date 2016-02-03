nVaR = function(x, p = 0.99, n = NULL){
  if (!is.null(n)){
    nr = nrow(x)
    return(qnorm(1-p, mean(x[(nr - n + 1):nr]), sd(x[(nr - n + 1):nr])))
  }else{
    return(qnorm(1-p, mean(x), sd(x)))
  }
}