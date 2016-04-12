rollFmSDdecomp = function(Fm.roll){
  rollSd = vector(mode = "list", length = length(Fm.roll))
  names(rollSd) = names(Fm.roll)
  for (i in 1:length(Fm.roll)){
    rollSd[[i]] = fmSdDecomp(Fm.roll[[i]])
  }
  return(rollSd)
}