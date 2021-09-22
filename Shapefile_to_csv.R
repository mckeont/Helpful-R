
#Shapefile to csv
library(rgdal)

leadpoints <- readOGR(dsn = "path/leadpoints", layer = "Lead_Data_CEET")

write.csv(leadpoints,
          file= "path/leadpoints.csv",
          row.names = FALSE)
