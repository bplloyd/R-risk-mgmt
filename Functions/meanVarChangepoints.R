meanVarChangepoints = function(R, penalty = "MBIC", pen.value = 0, method = "PELT", Q=5, test.stat = "Normal", class = T, param.estimates = T, shape = 1, minseglen = 2)
{
  R.ts = as.ts(R)
  cpt = cpt.meanvar(data = R.ts, penalty = penalty, pen.value = pen.value, method = method, Q=Q, test.stat = test.stat, class = class, param.estimates = param.estimates, shape = shape, minseglen = minseglen)
  return(index(R[cpt@cpts]))
}

varChangepoints =  function(R, penalty = "MBIC", pen.value = 0, know.mean = F, mu = NA,  method = "PELT", Q=5, test.stat = "Normal", class = T, param.estimates = T, minseglen = 2)
{
  R.ts = as.ts(R)
  cpt = cpt.var(data = R.ts, penalty = penalty, pen.value = pen.value, know.mean = know.mean, mu = mu,method = method, Q=Q, test.stat = test.stat, class = class, param.estimates = param.estimates, minseglen = minseglen)
  return(index(R[cpt@cpts]))
}