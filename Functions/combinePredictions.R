combinePredictions = function(model, newData)
{
    model.pred = predict(model, newData)
    result = as.xts(unlist(model.pred[[1]]))
    for(i in 2:length(model.pred))
    {
        result = cbind(result, as.xts(unlist(model.pred[[i]])))
    }
    names(result) = names(model.pred)
    return(result)
}