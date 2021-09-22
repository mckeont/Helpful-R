
#Shapefile to csv
library(rgdal)

leadpoints <- readOGR(dsn = "C:/Users/path/Documents/TREES/leadpoints", layer = "Lead_Data_CEET")

write.csv(leadpoints,
          file= "C:/Users/Tom McKeon/Documents/TREES/leadpoints.csv",
          row.names = FALSE)
