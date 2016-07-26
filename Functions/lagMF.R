lagMF = function(R)
{
  require(xts)
  require(stats)
  R_new = stats::lag(R, -1)['/20160615',]
  R_new = rbind(R_new, R['20160616/',])
                
  r_617 = xts(data.frame(Row = ((1+as.numeric(R$Row['20160617']))^0.5 - 1), 
                         Revolution = ((1+as.numeric(R$Revolution['20160617']))^0.5 - 1)),
              order.by = as.Date('2016-06-17'))        
  
  r_616 = xts(r_617, order.by = as.Date('2016-06-16'))       
  
 
  R_new['20160616/', c("Row", "Revolution")] = r_616
  R_new['20160617/', c("Row", "Revolution")] = r_617
  return(R_new)
}