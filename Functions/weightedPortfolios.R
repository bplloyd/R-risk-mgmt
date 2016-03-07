weightedPortfolios = function(subs.o, subs.weights)
{
    weights = subs.weights[names(subs.o)]
    result = xts(mapply(FUN = function(x,y){R = x[, names(y)]; w = y; return(xts(apply(R, 1, FUN = function(r)return(sum(r*w))), order.by = index(R)))},  subs.o, weights), order.by = index(subs.o$LSE))
    names(result) = names(subs.o)
    return(result)
}
