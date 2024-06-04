setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Clean Data")

deer.cs = read.csv("deer_checksample_2014-2023.csv")
IL.county = read.csv("IL_map_data.csv")

deer.check.tot = aggregate(deer.cs, checked ~ year + county, sum)
deer.sample.tot = aggregate(deer.cs, sampled ~ year + county, sum)

deer.cs.tot = cbind(deer.check.tot, sampled = deer.sample.tot$sampled, 
                    prop = deer.sample.tot$sampled/deer.check.tot$checked)

ggplot(deer.cs.tot) + geom_histogram(aes(x = prop)) +
  geom_vline(aes(xintercept = mean(prop)), colour="red", lwd = 3.0) + theme_cowplot() +
  ggtitle("average testing rate per year and county")


deer.cs.mean = aggregate(deer.cs.tot, prop ~ county, mean)

ggplot(deer.cs.mean) + geom_histogram(aes(x = prop)) +
  geom_vline(aes(xintercept = mean(prop)), colour="red", lwd = 3.0) + theme_cowplot() + 
  ggtitle("aggregated by county: average testing rate")


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
cs.out = NULL
for(i in 1:dim(IL.county)[1]){
  mean.test.rate = NA
  
  county.need = IL.county$subregion[i]
  mean.index = which(deer.cs.mean == county.need)
  if(length(mean.index)>0){
    mean.test.rate = deer.cs.mean$prop[mean.index]
  }
  cs.out = c(cs.out, mean.test.rate)
}

IL.county.mean = cbind(IL.county, test.rate = cs.out)

il_base + 
  geom_polygon(data = IL.county.mean, aes(fill = test.rate), color = "black") + 
  theme(legend.position = "right") + scale_fill_gradient("average testing rate\nper year", low = "dodgerblue", high = "firebrick")

