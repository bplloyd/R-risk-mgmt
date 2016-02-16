emaCross = function(data, nfast, nslow, side)
{
    if(side == 'l')
        signal = lag(ifelse(EMA(data, nfast) >= EMA(data, nslow), 1, 0))
    if(side == 's')
        signal = lag(ifelse(EMA(data, nfast) >= EMA(data, nslow), 0, -1))
    if(side == 'b')
        signal = lag(ifelse(EMA(data, nfast) > EMA(data, nslow), 1,  -1))
    return(CalculateReturns(data)*signal)
}