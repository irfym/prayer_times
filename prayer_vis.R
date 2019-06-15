library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(zoo)

#Load in prayer times and convert to tidy format
prayer_times <- read.csv("./data/prayertimes.csv", stringsAsFactors = FALSE)

#Fix missing values to use last valid value
for (col in names(prayer_times)) {
  prayer_times[,col][prayer_times[,col]=="** **"] = NA
  prayer_times[,col] = na.locf(prayer_times[,col])
}



#Change to long data format/tidy format
prayer_times <- gather(data=prayer_times, 'Twilight','Sunrise','Transit','Shadow.1','Shadow.2','Sunset','Twilight.1',
                       key = 'time', value = 'value')
