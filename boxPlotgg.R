
library(ggplot2)
startDate = '201601/'
dt = data.table(R[startDate]*100)
dt.m = melt(dt, variable.name = "Name", value.name = "DailyReturn") 
p = ggplot(dt.m, aes(y=DailyReturn, x = Name, fill = Name)) + geom_boxplot() + scale_fill_brewer(palette = 3)
p = p + ggtitle("Since Jan 2016")
p = p + ylim(-6, 5)
p
