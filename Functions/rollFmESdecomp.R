rollFmESdecomp = function(Fm.roll, p = 0.99){
  rollES = vector(mode = "list", length = length(Fm.roll))
  names(rollES) = names(Fm.roll)
  for (i in 1:length(Fm.roll)){
    rollES[[i]] = fmEsDecomp(Fm.roll[[i]], p = p)
  }
  return(rollES)
}