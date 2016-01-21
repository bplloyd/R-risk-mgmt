require(Quandl)
source("Quandl.key.R")
Quandl.api_key(quandl.key)
path =  file.path('QuandlData', 'dataset_mlfi.csv')
#path =  file.path('QuandlData', 'dataset_mlfi.csv')
read.table(file = path, header = T, sep = ",") -> mlfi.codes
mlfi.codes = paste(mlfi.codes$database_code, mlfi.codes$dataset_code, sep = '/')
lapply(mlfi.codes, function(x)return(Quandl(x, type = "xts"))) -> mlfi.data
mlfi.xts = mlfi.data[[1]]
for(i in 2:length(mlfi.data)){mlfi.xts = merge.xts(mlfi.xts, mlfi.data[[i]])}