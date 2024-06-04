require(ggplot2)
require(cowplot)

setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Raw Data")
harvest.data = read.csv("2005-2022_HarvestData_Vinson.csv", header = TRUE)
setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Clean Data")
county.data = read.csv("ILCounties.csv", header = TRUE)

harvest.data$HarvCounty = trimws(harvest.data$HarvCounty)
harvest.data$HarvCounty[which(harvest.data$HarvCounty == "ST CLAIR")] = "ST. CLAIR"
harvest.data$HarvCounty[which(harvest.data$HarvCounty == "SAINT CLAIR")] = "ST. CLAIR"
harvest.data$HarvCounty[which(harvest.data$HarvCounty == "JODAVIESS")] = "JO DAVIESS"

harvest.agg.year = aggregate(harvest.data, Total~Year, sum)
harvest.agg.year.county = aggregate(harvest.data, Total ~ Year + HarvCounty, sum)
harvest.mean.year.county = aggregate(harvest.agg.year.county, Total ~ Year, mean)

area.vec = NULL
for(i in 1:dim(harvest.agg.year.county)[1]){
  county.need = harvest.agg.year.county$HarvCounty[i]
  
  area.need = county.df$area[which(county.df$county == county.need)]
  area.vec = c(area.vec, area.need)
}

harvest.data = data.frame(year = harvest.agg.year.county$Year, county = harvest.agg.year.county$HarvCounty,
                          area.mi = area.vec, area.km = area.vec*1.61, harv.tot = harvest.agg.year.county$Total)

setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Clean Data")
write.csv(harvest.data, "harvest_05-22_data.csv", row.names = FALSE)
