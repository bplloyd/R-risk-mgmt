organizeSubs = function(subs=NULL, curMgrs = T, allocations = NULL){
  if(is.null(subs))
      subs = loadSubAdvisors()
  if(curMgrs)
  {
      if(is.null(allocations))
          allocations = getAllocations(end(subs))
      
      cols.lse = which(names(subs) %in% allocations$LSE$Name)
      cols.mn = which(names(subs) %in% allocations$MN$Name)
      cols.ed = which(names(subs) %in% allocations$ED$Name)
      cols.lsd = which(names(subs) %in% allocations$LSD$Name)
      cols.mf = which(names(subs) %in% allocations$MF$Name)
  }
  if(!curMgrs)
  {
    cols.lse = which(names(subs) %in% c('Apis', 'BlueJay', 'BoardmanBay', 'Coe', 'ISF', 'LoremIpsum','MiscLSE'))
    cols.mn = which(names(subs) %in% c('Jadwin', 'Longbow', 'Nicholas', 'MiscMN'))
    cols.ed = which(names(subs) %in% c('FrontFour','Havens', 'Moab', 'Mountaineer','MiscED', 'WhiteOak'))
    cols.lsd = which(names(subs) %in% c('Lutetium','MatlinPatterson', 'MeehanCombs', 'MiscLSD', 'Phoenix', 'RavenRock',  'SmithBreeden', 'Soundpoint'))
    cols.mf = which(names(subs) %in% c('Centurion', 'Dominion', 'MiscMF','Revolution', 'Row'))
  }
  return(list(ED = subs[, cols.ed], LSE = subs[, cols.lse], LSD = subs[, cols.lsd], MF = subs[, cols.mf], MN = subs[, cols.mn]))
}