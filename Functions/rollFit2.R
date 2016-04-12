rollFit = function(data, formula, width = 63, fitter = "lm", on = "days"){
  fitter = match.fun(fitter)
  data = na.omit(data)
  slices = createTimeSlices2(data, initialWindow = width, on = on)
  #results = vector(mode = "list", length = length(slices[[1]]))
  return(lapply(slices, FUN = function(l)return(fitter(formula = formula, data=data[l,]))))
}