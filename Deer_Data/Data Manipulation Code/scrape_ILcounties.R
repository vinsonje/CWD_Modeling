require(rvest)
require(stringr)

setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Raw Data")

webpage = read_html("https://en.wikipedia.org/wiki/List_of_counties_in_Illinois#cite_note-NACO-2") 

# Select the table using CSS selector 
table_node = html_nodes(webpage, "table") 

# Extract the table content 
table_content = as.data.frame(html_table(table_node)[[2]])

# Print the table 
county.df = data.frame(county = table_content$County, area = table_content$`Area[2]`)
county.df$county = unlist(strsplit(county.df$county, split = " County"))
county.df$county = str_to_upper(county.df$county)
county.df$area = str_trim(unlist(str_split_i(county.df$area, pattern = "sq", 1)))
county.df$area = as.numeric(sub(",", "", county.df$area))

setwd("C:/Users/SIU856560341/Desktop/CWD_Modeling/Deer_Data/Clean Data")
write.csv(county.df, "ILCounties.csv", row.names = FALSE)
                        