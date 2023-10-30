
#Shapefile to csv
library(rgdal)

shapefile <- readOGR(dsn = "path/shapefilefolder", layer = "shapefile")

write.csv(shapefile,
          file= "path/shapefile.csv",
          row.names = FALSE)

dataset <- read.csv("path/shapefile.csv", header = TRUE)
          
