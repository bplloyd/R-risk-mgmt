rollFmSDdecomp = function(Fm.roll){
  rollSD = vector(mode = "list", length = length(Fm.roll))
  names(rollSD) = names(Fm.roll)
  for (i in 1:length(Fm.roll)){
    rollSD[[i]] = fmSdDecomp(Fm.roll[[i]])
  }
  return(rollSD)
}