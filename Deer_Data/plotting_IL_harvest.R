library(ggplot2)
library(maps)
library(ggmap)

setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Clean Data")

harvest.data = read.csv("harvest_05-22_data.csv")
IL.county = read.csv("IL_map_data.csv")

harv.area.mi = harvest.data$harv.tot/harvest.data$area.mi

harv.area.km = harvest.data$harv.tot/harvest.data$area.km

harvest.data2 = cbind(harvest.data, harv.area.mi, harv.area.km)

ggplot(harvest.data2) + geom_col(aes(x = county, y = harv.area.km)) + theme_cowplot() + 
  facet_wrap(~year)


harvest.mean = aggregate(harvest.data2, harv.area.km ~ county, mean) 

ggplot(harvest.mean) + geom_histogram(aes(x = harv.area.km)) +
  geom_vline(aes(xintercept = mean(harvest.mean$harv.area.km)), colour="red", lwd = 3.0) + theme_cowplot()

#Plot IL counties
states = map_data("state")
IL = subset(states, region %in% c("illinois"))

il_base = ggplot(data = IL, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = NA) + 
  theme_nothing()

il_base + 
  geom_polygon(data = IL.county, fill = NA, color = "black") +
  geom_polygon(color = "black", fill = NA) 

#combine map and harv data (mean)
mean.km = NULL
for(i in 1:dim(IL.county)[1]){
  county.need = IL.county$subregion[i]
  mean.index = which(harvest.mean$county == county.need)
  mean.km = c(mean.km, harvest.mean$harv.area.km[mean.index])
}

IL.county.mean = cbind(IL.county, mean.km = mean.km)

il_base + 
  geom_polygon(data = IL.county, aes(fill = mean.km), color = "black") + 
  theme(legend.position = "right") + scale_fill_gradient("average harvest/KM\nper year", low = "dodgerblue", high = "firebrick")

