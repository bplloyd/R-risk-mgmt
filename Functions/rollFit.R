rollFit = function(data, formula, slices, fitter = lm){
  results = vector(mode = "list", length = length(slices[[1]]))
  return(lapply(slices, FUN = function(l)return(fitter(formula = formula, data=data[l,]))))
}