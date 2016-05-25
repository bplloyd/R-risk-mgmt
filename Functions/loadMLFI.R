loadMLFI = function(){
  require(Quandl)
  source("Functions/Quandl.key.R")
  Quandl.api_key(quandl.key)
  path =  file.path('QuandlData', 'dataset_mlfi.csv')
  #path =  file.path('QuandlData', 'dataset_mlfi.csv')
  read.table(file = path, header = T, sep = ",") -> mlfi.codes
  names = mlfi.codes$dataset_code
  mlfi.codes = paste(mlfi.codes$database_code, mlfi.codes$dataset_code, sep = '/')
  return(Quandl(code = mlfi.codes, type="xts"))
}