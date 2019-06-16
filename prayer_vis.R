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

#Rename columns and change to long data format/tidy format
#Add factor levels for prayer names
names(prayer_times) <- c("Year","mth","d","Fajr","Sunrise","Dhuhr","Asr.1","Asr.2","Sunset","Isha")
prayer_times <- gather(data=prayer_times, 'Fajr','Sunrise','Dhuhr','Asr.1','Asr.2','Sunset','Isha',
                       key = 'name', value = 'time')
prayer_times$name <- factor(x=prayer_times$name,c("Isha","Sunset","Asr.2","Asr.1","Dhuhr","Sunrise","Fajr"))

#Add helper column to move any 24+ hr times to next day
prayer_times$next_day <- ifelse(as.numeric(str_sub(prayer_times$time,1,2)) >= 24,1,0)

#Concatenate columns and split out date and time for plotting, tidy up table
prayer_times$date <- paste(prayer_times$Year, prayer_times$mth, prayer_times$d, sep="-")
prayer_times$date_time <- ymd_hm(paste(prayer_times$date, prayer_times$time, sep="T"))
prayer_times <- prayer_times %>%
  mutate(time=format(date_time, format="%H;%M:%S")) %>%
  mutate(time=as.POSIXct(time, format="%H;%M:%S") + (prayer_times$next_day * 24* 3600)) %>%
  select(-c("Year","mth","d","next_day","date"))

#Plot onto line chart
prayer_chart <- ggplot(data=prayer_times,aes(x=date_time,y=time,colour=name)) +
  geom_line() +
  scale_x_datetime(name="Month",date_labels="%b", date_breaks = "1 month", date_minor_breaks = "1 month") + 
  scale_y_datetime(name="Time",date_labels="%H:%M", date_breaks = "1 hours", date_minor_breaks = "1 hour") +
  scale_colour_discrete(name="",label=c("Isha","Sunset","Asr 2","Asr 1","Dhuhr","Sunrise","Fajr"))

#Write image to file
ggsave("./outputs/prayer_chart.png",device = "png",height = 12, width = 18, units="cm", dpi=600)

############################
#To debug:
#General visual touch-ups



