setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Raw Data")

CWD.03_22 = read.csv("IL_CWD_2003-2022.csv")
CWD.23 = read.csv("IL_CWD_2023.csv")

CWD.all = rbind(CWD.03_22, CWD.23)

names(CWD.all) = c("date", "county", "age", "sex", "status", "source", "collection.method", "group", "trs")

CWD.all$county = str_to_upper(CWD.all$county)
CWD.all$county[which(CWD.all$county == "JODAVIESS")] = "JO DAVIESS"
CWD.all$county[which(CWD.all$county == "STCLAIR")] = "ST. CLAIR"
CWD.all$county[which(CWD.all$county == "ROCKISLAND")] = "ROCK ISLAND"

CWD.all$date[which(nchar(CWD.all$date) == 7)] = sapply("0", paste, CWD.all$date[which(nchar(CWD.all$date) == 7)], sep="")

sub.slash = function(text){paste(c(substr(text, 1, 2), substr(text, 3,4), substr(text, 5,8)), collapse="/")}
CWD.all$date = sapply(CWD.all$date, sub.slash)

CWD.all$date = as.Date(CWD.all$date, "%m/%d/%Y")

wrong.year = which(year(CWD.all$date) > 2030)
year.2111 = which(year(CWD.all$date) == 2111)
year(CWD.all$date[year.2111]) = 2011
year.3013 = which(year(CWD.all$date) == 3013)
year(CWD.all$date[year.3013]) = 2013

setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Clean Data")

write.csv(CWD.all, "CWD_data_2003-2023.csv", row.names = FALSE)
