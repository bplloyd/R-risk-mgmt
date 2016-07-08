Return.excess_edit = function (R, Rf = 0) 
{
  R = checkData(R)
  if (!is.null(dim(Rf))) {
    Rf = checkData(Rf)
    coln.Rf = colnames(Rf)
    if (is.null(coln.Rf)) {
      colnames(Rf) = "Rf"
      coln.Rf = colnames(Rf)
    }
    Rft = cbind(R, Rf)
    Rft = na.locf(Rft[, make.names(coln.Rf)])
    Rf = Rft[which(index(Rft) %in% index(R))]
  }
  else {
    coln.Rf = "Rf"
    Rf = reclass(rep(Rf, length(index(R))), R)
  }
  result = do.call(merge, lapply(1:NCOL(R), function(nc) R[, 
                                                           nc] - coredata(Rf)))
  if (!is.null(dim(result))) 
    colnames(result) = paste(colnames(R), ">", coln.Rf)
  return(result)
}


Rc = checkData(R)
R=lse
Rf = sp2
if (!is.null(dim(Rf))) {
  Rf = checkData(Rf)
  coln.Rf = colnames(Rf)
  if (is.null(coln.Rf)) {
    colnames(Rf) = "Rf"
    coln.Rf = colnames(Rf)
  }
  Rft = cbind(R, Rf)
  Rft = na.locf(Rft[, make.names(coln.Rf)])
  Rf = Rft[which(index(Rft) %in% index(R))]
}
else {
  coln.Rf = "Rf"
  Rf = reclass(rep(Rf, length(index(R))), R)
}
result = do.call(merge, lapply(1:NCOL(R), function(nc) R[, 
                                                         nc] - coredata(Rf)))
if (!is.null(dim(result))) 
  colnames(result) = paste(colnames(R), ">", coln.Rf)
return(result)