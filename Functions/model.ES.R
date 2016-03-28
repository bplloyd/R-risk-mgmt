model.ES = function(R, weights, model, newData, p)
{
    newReturns = combinePredictions(model, newData)
    newReturns = apply(newReturns[,names(R)], 1, function(x)return(t(weights[names(R)]) %*% x))
    return(ES(newReturns, p = p))
}