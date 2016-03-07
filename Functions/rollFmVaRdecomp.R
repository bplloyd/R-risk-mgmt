rollFmVaRdecomp = function(Fm.roll, p = 0.98){
  rollVaR = vector(mode = "list", length = length(Fm.roll))
  names(rollVaR) = names(Fm.roll)
  for (i in 1:length(Fm.roll)){
    rollVaR[[i]] = fmVaRDecomp(Fm.roll[[i]], p = p)
  }
  return(rollVaR)
}