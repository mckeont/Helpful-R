
install.packages("rgdal")
library(rgdal) 

leadpoints <- readOGR(dsn = "C:/Users/Tom McKeon/Documents/TREES/leadpoints", layer = "Lead_Data_CEET_Aug21_2019")

write.csv(leadpoints,
          file= "C:/Users/Tom McKeon/Documents/TREES/leadpoints.csv",
          row.names = FALSE)

leadpoints