library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#Uploading Dataset
q1_2019 <- read_csv("Divvy_Trips_2019_Q1 - Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv")

colnames(q1_2019)
colnames(q1_2020)

#Renaming columns in 2019 to match 2020
q1_2019 <- rename(q1_2019, 
                  ride_id = trip_id
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name
                  ,end_station_name = to_station_name
                  ,rideable_type = bikeid
                  ,start_station_id = from_station_id
                  ,end_station_id = to_station_id
                  ,member_casual = usertype)

#Inspecting dataframe to look for any incongurencies
str(q1_2019)
str(q1_2020)

#Converting ride_id and rideable_type to characters to make data stack well
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

#Stacking both data into One big dataframe
all_trips <- bind_rows(q1_2019, q1_2020)

#Removing unnecessary columns
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,tripduration))



#DATA CLEANING PROCESS

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

#Consolidating member_casual column into two instead of four
table(all_trips$member_casual)
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual, "subscriber" = "member"
                                ,"customer" = "casual"))
table(all_trips$member_casual)  


#Adding new columns(Date, Month, Day and Year) to each ride
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#Adding ride length calculations to data
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
str(all_trips)

#Converted ride_length to numeric to run calculations on data
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
str(all_trips)

#Removing bad/unwanted data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


#Conducting Descriptive analysis on data
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)
summary(all_trips_v2$ride_length)

#Comparing Members and Casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#Checking average ride time by each day for members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#Putting day of week the week in order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Average ride time by each day between casual users and members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#Analyzing ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

#Visualizing the number of rides by rider type 
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")

ggplot(data = all_trips_v2, mapping =geom_point(aes(x = weekday, y = number_of_rides))
       
       

       

