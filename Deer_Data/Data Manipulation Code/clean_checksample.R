setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Raw Data")

checked = read.csv("deer_checked_2014-2023.csv")
names(checked) = str_to_lower(sub("X2", "2", names(checked)))
checked.deer = gather(checked, key = "year", value = "checked", "2014":"2023")

sampled = read.csv("deer_sampled_2014-2023.csv")
names(sampled) = str_to_lower(sub("X2", "2", names(sampled)))
sampled.deer = gather(sampled, key = "year", value = "sampled", "2014":"2023")

deer.check.sample = cbind(checked.deer, sampled = sampled.deer$sampled)

setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Clean Data")
write.csv(deer.check.sample, "deer_checksample_2014-2023.csv", row.names = FALSE)
