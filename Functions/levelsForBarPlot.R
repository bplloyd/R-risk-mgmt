levelsForBarPlot = function(levels)
{
  library(data.table)
  library(stringr)
  levels = data.table(fund = row.names(levels), levels)
  levels = melt(levels, id.vars = c(1), measure.vars = c(2,3,4,5),variable.name = "Date", value.name = "ExpectedTailLoss")
  levels$Date = as.character.factor(levels[,Date])
  levels$Date = str_sub(levels[, Date], start = str_locate(levels[,Date], pattern = " ")[,1]+1, end = str_length(levels[,Date]))
  #levels$Date = as.Date.character(levels$Date)
  #format = "%Y/%m/%d"
  return(levels)
}

