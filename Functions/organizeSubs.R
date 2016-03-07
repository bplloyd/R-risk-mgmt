organizeSubs = function(subs=NULL){
  if(is.null(subs))
      subs = loadSubAdvisors()
  cols.lse = which(names(subs) %in% c('Apis', 'BlueJay', 'BoardmanBay', 'Coe', 'ISF','MiscLSE'))
  cols.mn = which(names(subs) %in% c('Jadwin', 'Longbow',  'MiscMN'))
  cols.ed = which(names(subs) %in% c('FrontFour', 'Havens','MiscED','Mountaineer'))
  cols.lsd = which(names(subs) %in% c('MatlinPatterson', 'MiscLSD', 'SmithBreeden', 'Soundpoint'))
  cols.mf = which(names(subs) %in% c('MiscMF','Revolution', 'Row'))
  return(list(ED = subs[, cols.ed], LSE = subs[, cols.lse], LSD = subs[, cols.lsd], MF = subs[, cols.mf], MN = subs[, cols.mn]))
}