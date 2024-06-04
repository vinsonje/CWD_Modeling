library(tidyverse)

setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Clean Data")

CWD.data = read.csv("CWD_data_2003-2023.csv")
CWD.data.year = year(CWD.data$date)
CWD.data.month = month(CWD.data$date)
CWD.data.day = day(CWD.data$date)
CWD.data = cbind(CWD.data, year = CWD.data.year, month = CWD.data.month, day = CWD.data.day)

CWD.ss.data = subset(CWD.data, group %in% "SHARPSHOOTING")

CWD.ss.counts = data.frame(table(CWD.ss.data$date))
names(CWD.ss.counts) = c("date", "ss.removal")
CWD.ss.counts$date = as.Date(CWD.ss.counts$date, "%Y-%m-%d")

ggplot(CWD.ss.counts) + geom_point(aes(x = date, y = ss.removal)) + 
  scale_x_date(date_labels = "%b %Y") + theme_cowplot()

CWD.ss.month.year = data.frame(table(CWD.ss.data$year, CWD.ss.data$month))
names(CWD.ss.month.year) = c("year", "month", "ss.removal")
CWD.ss.month.year = CWD.ss.month.year[-which(CWD.ss.month.year$ss.removal == 0),]
CWD.ss.month.year$date = as.yearmon(paste(CWD.ss.month.year$year, CWD.ss.month.year$month), "%Y %m")

ggplot(CWD.ss.month.year) + geom_point(aes(x = date, y = ss.removal)) + theme_cowplot()

CWD.ss.type = CWD.ss.data %>% select('year','month','collection.method') %>% ftable()
CWD.ss.type = as.data.frame(CWD.ss.type)
names(CWD.ss.type) = c("year", "month", "collection.method", "ss.removal")
CWD.ss.type = CWD.ss.type[-which(CWD.ss.type$ss.removal == 0),]
CWD.ss.type$date = as.yearmon(paste(CWD.ss.type$year, CWD.ss.type$month), "%Y %m")

ggplot(CWD.ss.type) + geom_point(aes(x = date, y = ss.removal, col = collection.method)) + theme_cowplot() +
  theme(legend.position = "bottom") + geom_smooth(aes(x = date, y = ss.removal, col = collection.method))

CWD.ss.avg = aggregate(CWD.ss.type, ss.removal ~ month + collection.method, mean)

ggplot(CWD.ss.avg) + geom_col(aes(x = month, y = ss.removal, fill = collection.method), position = "dodge") + theme_cowplot() +
  theme(legend.position = "bottom")
