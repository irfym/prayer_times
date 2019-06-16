library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(zoo)

#Load in prayer times and convert to tidy format
prayer_times <- read.csv("./data/prayertimes.csv", stringsAsFactors = FALSE)

#Fix missing values to use last valid value, add dot in time value
for (col in names(prayer_times)) {
  prayer_times[,col][prayer_times[,col]=="** **"] = NA
  prayer_times[,col] = na.locf(prayer_times[,col])
  prayer_times[,col] = gsub(" ",":",prayer_times[,col])
}

#Change to long data format/tidy format
prayer_times <- gather(data=prayer_times, 'Twilight','Sunrise','Transit','Shadow.1','Shadow.2','Sunset','Twilight.1',
                       key = 'name', value = 'time')

#Add helper column to move any 24+ hr times to next day
prayer_times$next_day <- ifelse(as.numeric(str_sub(prayer_times$time,1,2)) >= 24,1,0)

#Concatenate columns and split out date and time for plotting
prayer_times$date <- paste(prayer_times$Year, prayer_times$mth, prayer_times$d, sep="-")
prayer_times$date_time <- ymd_hm(paste(prayer_times$date, prayer_times$time, sep="T"))

prayer_times <- prayer_times %>%
  mutate(time=format(date_time, format="%H;%M:%S")) %>%
  mutate(time=as.POSIXct(time, format="%H;%M:%S") + (prayer_times$next_day * 24* 3600)) %>%
  select(-c("Year","mth","d","next_day","date"))

#Plot onto line chart
prayer_chart <- ggplot(data=prayer_times,aes(x=date_time,y=time,colour=name)) +
  geom_line()

############################
#To debug:
#Order of each prayer in the graph
#Date showing on left hand column - there must be a better way of showing time
#General visual touch-ups



