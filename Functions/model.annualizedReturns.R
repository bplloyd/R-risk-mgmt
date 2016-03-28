model.annualizedReturn = function(R, weights, model, newData)
{
    newReturns = combinePredictions(model, newData)
    newReturns = apply(newReturns[,names(R)], 1, function(x)return(t(weights[names(R)]) %*% x))
    return(Return.annualized(newReturns))
}