#####################################
#GPS Location Filter
#####################################

location_filter = function(gps.data, xmin, xmax, ymin, ymax){
  
  outside.ymax = which(gps.data$Y>ymax)
  outside.ymin = which(gps.data$Y<ymin)
  
  outside.xmin = which(gps.data$X<xmin)
  outside.xmax = which(gps.data$X>xmax)
  
  all.outside = union(outside.xmax, outside.xmin)
  all.outside = union(all.outside, outside.ymax)
  all.outside = union(all.outside, outside.ymin)
  
  gps.data.inrange = gps.data[-all.outside,]
  
  return(gps.data.inrange)
}


###############
#Test
###############

# setwd("F:/GPS") #PC
# # setwd("~/Desktop/GPS_Analysis/GPS") #MAC
# 
# allgpsdata = read.csv("allgpsdata_small.csv", header = TRUE)
# 
# #carbondale
# ymin = 37.6 #bottom left
# ymax = 37.7 #top right
# xmax = -89.1 # top right
# xmin = -89.2 #bottom left
# 
# loc.fil.test = location_filter(allgpsdata, xmin, xmax, ymin, ymax)