getFamaQuandl = function(){
  require(Quandl)
  require(xts)
  Quandl.api_key(getQuandlKey())
  f5 = Quandl(code="KFRENCH/FACTORS5_D", type = "xts")
  m = Quandl(code="KFRENCH/MOMENTUM_D", type = "xts")
  names(m) = "MOM"
  return(cbind(f5, m))
}