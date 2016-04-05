plotExposure = function(id, label)
{
    id = 58
    label = "Coe"
    library(ggplot2)
    exp = sectorExposure(id, on = "weeks")
    dt = melt(data.table(data.frame(Date = as.Date(index(exp)), na.fill(exp, fill = 0 ), row.names = NULL)), 'Date')
    names(dt)[2:3] = c("Sector", "Exposure")
    setkey(dt, Sector)
    dt$Pos = as.factor(ifelse(dt$Exposure >= 0, "L", "S"))
    dt.def = dt[c("Consumer.Staples", "Health.Care", "Utilities", "Energy", "Telecommunication.Services")]
    dt.cyc = dt[c("Consumer.Discretionary", "Financials", "Industrials", "Information.Technology", "Materials")]
    
    area.all = ggplot(dt, aes(x=Date, y=Exposure, fill=Sector)) + scale_x_date() 
    area.all = area.all + geom_area(colour = "darkgray", alpha = 0.35, size = 0.5) 
    #area.all = area.all + scale_fill_brewer( breaks = rev(levels(dt$Sector)))
    area.all = area.all + ggtitle(paste(label, "All Sector Exposures", sep = " "))
    area.all = area.all + theme_bw()
    
    area.cyc = ggplot(dt.cyc, aes(x=Date, y=Exposure, fill=Sector)) + scale_x_date()
    area.cyc = area.cyc + geom_area(colour = "gray", alpha = 0.35, size = 0.5) 
    area.cyc = area.cyc + scale_fill_brewer()
    area.cyc = area.cyc + ggtitle(paste(label, "Cyclical Sector Exposures", sep = " "))
    area.cyc  = area.cyc + theme_bw()
    
    facet.cyc = ggplot(dt.cyc,aes(x=Date,y=Exposure, fill = Pos)) 
    facet.cyc = facet.cyc + geom_area(colour = "gray", alpha = 0.35, size = 0.5) 
    facet.cyc = facet.cyc + facet_grid(Sector ~ .)
    facet.cyc = facet.cyc + theme_bw()
    facet.cyc = facet.cyc + scale_fill_manual(values = c("blue", "red"), guide = F)
    facet.cyc = facet.cyc + ggtitle(paste(label, "Cyclical Sector Exposures", sep = " "))
    
    area.def = ggplot(dt.def, aes(x=Date, y=Exposure, fill=Sector)) + scale_x_date()
    area.def = area.def + geom_area(colour = "gray", alpha = 0.35, size = 0.5) 
    area.def = area.def + scale_fill_brewer()
    area.def = area.def + ggtitle(paste(label, "Defensive Sector Exposures", sep = " "))
    area.def = area.def + theme_bw()
    
    facet.def = ggplot(dt.def,aes(x=Date,y=Exposure, fill = Pos)) 
    facet.def = facet.def + geom_area(colour = "gray", alpha = 0.35, size = 0.5) 
    facet.def = facet.def + facet_grid(Sector ~ .)
    facet.def = facet.def + theme_bw()
    facet.def = facet.def + scale_fill_manual(values = c("blue", "red"), guide = F)
    facet.def = facet.def + ggtitle(paste(label, "Defensive Sector Exposures", sep = " "))
  
    
    # area.cyc
    facet.cyc
    # area.def
    facet.def
    # area.all
}