


plotBounds = function(returns){
  require(ggplot2)
  data = merge.xts(na.omit(returns), na.omit(nVaRBoundsRolling(returns)))
  df = data.frame(Date = as.Date(index(data)), Returns = data[,1], LowBound = data[,2], UBound = data[,3], row.names = NULL) 
  p = ggplot(df, aes(x=Date, y = df[,2]), environment = environment()) 
  p = p + geom_bar(stat = "identity", colour = "black",width = 1) 
  p = p + geom_line(aes(y=df[,3], colour = names(df)[3]), size = 1) 
  p = p + geom_line(aes(y=df[,4], colour = names(df)[4]), size = 1)
  p = p + theme_light()
  p = p + scale_color_manual("", breaks = c(names(df)[4],names(df)[3]), values = c("green","red"))
  p = p + labs(title = paste(names(df)[2], sep = " "))
  p = p + ylab(paste(names(df)[2],sep = ""))
  return(p)
}