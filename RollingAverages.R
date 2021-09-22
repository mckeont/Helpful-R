##load packages
library(plyr)
library(lubridate)
library(zoo)

#read the data
data <- read.csv("data.csv", header = TRUE)

##Make sure date is a date
data$date <- as.Date(data$date)

## Monthly means:
output <- aggregate(data$x, list(format(data$date, "%Y-%m"), data$plant), mean)
colnames(output) <- c('monthYear', 'plant', 'monthlyValue')
print(output)  ## Check

## Rolling average of each location individually:
Queen_Lane <-subset(output, plant == "Queen Lane" , select = c("monthYear", "plant", "monthlyValue"), all = TRUE)
Baxter <-subset(output, plant == "Baxter" , select = c("monthYear", "plant", "monthlyValue"), all = TRUE)
Belmont <-subset(output, plant == "Belmont" , select = c("monthYear", "plant", "monthlyValue"), all = TRUE)
Mix <-subset(output, plant == "Baxter/Queen Lane Mix" , select = c("monthYear", "plant", "monthlyValue"), all = TRUE)

##rollapply. width four months. Change width for rolling averages by different time periods
queenlane <- rollapply(Queen_Lane$monthlyValue, width = 4, FUN = mean)
baxter <- rollapply(Baxter$monthlyValue, width = 4, FUN = mean)
belmont <- rollapply(Belmont$monthlyValue, width = 4, FUN = mean)
mix <- rollapply(Mix$monthlyValue, width = 4, FUN = mean)

## convert to dataframe...
Queen_Lane_roll <- as.data.frame(queenlane)
Baxter_roll <- as.data.frame(baxter)
Belmont_roll <- as.data.frame(belmont)
Mix_roll <- as.data.frame(mix)

## Bring them back together
together <- cbind(Baxter_roll,Queen_Lane_roll,Belmont_roll,Mix_roll)

## Write out a csv file. It doesn't include a date column. but the order should be jan-april 2013, feb - may 2013, march - june 2013, etc
write.csv (together,
           file = "C:/somewhere/rollingaverage_andrea.csv",
           row.names = FALSE)
