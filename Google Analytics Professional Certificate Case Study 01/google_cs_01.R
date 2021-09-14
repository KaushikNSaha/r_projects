# Google Capstone Case Study 01

# Setup ------------------------------------------------------------------

library(tidyverse)
library(DataExplorer)
library(janitor)
library(lubridate)


# load data --------------------------------------------------------------

june20 <- read_csv("../Case_Study_01/tripdata/202006-divvy-tripdata.csv")

july20 <- read_csv("../Case_Study_01/tripdata/202007-divvy-tripdata.csv")

aug20 <- read_csv("../Case_Study_01/tripdata/202008-divvy-tripdata.csv")

sep20 <- read_csv("../Case_Study_01/tripdata/202009-divvy-tripdata.csv")

oct20 <- read_csv("../Case_Study_01/tripdata/202010-divvy-tripdata.csv")

nov20 <- read_csv("../Case_Study_01/tripdata/202011-divvy-tripdata.csv")

dec20 <- read_csv("../Case_Study_01/tripdata/202012-divvy-tripdata.csv")

jan21 <- read_csv("../Case_Study_01/tripdata/202101-divvy-tripdata.csv")

feb21 <- read_csv("../Case_Study_01/tripdata/202102-divvy-tripdata.csv")

mar21 <- read_csv("../Case_Study_01/tripdata/202103-divvy-tripdata.csv")

apr21 <- read_csv("../Case_Study_01/tripdata/202104-divvy-tripdata.csv")

may21 <- read_csv("../Case_Study_01/tripdata/202105-divvy-tripdata.csv")

# wrangle and combine data in a single data set -------------------------------------

# checking column name for any inconsistency 

colnames(june20)
colnames(july20)
colnames(aug20) 
colnames(sep20)
colnames(oct20)
colnames(nov20)
colnames(dec20)
colnames(jan21)
colnames(feb21)
colnames(mar21)
colnames(apr21)
colnames(may21)


str(june20)
str(july20)
str(aug20)
str(sep20)
str(oct20)
str(nov20)
str(dec20) # problem with start_station_id and end_station_id data type
str(jan21) # problem with start_station_id and end_station_id data type
str(feb21) # problem with start_station_id and end_station_id data type
str(mar21) # problem with start_station_id and end_station_id data type
str(apr21) # problem with start_station_id and end_station_id data type
str(may21) # problem with start_station_id and end_station_id data type


table(june20$start_station_id)
unique(june20$start_station_id)
table(dec20$start_station_id)
unique(june20$member_casual)

# convert start_station_id and end_station_id to double /  numeric

dec20 <- mutate(dec20, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))

jan21 <- mutate(jan21, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))

feb21 <- mutate(feb21, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))

mar21 <- mutate(mar21, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))

apr21 <- mutate(apr21, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))

may21 <- mutate(may21, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))


# bind all the rows together

trip <- bind_rows(june20, july20, aug20, sep20, oct20,
                  nov20, dec20, jan21, feb21, mar21,
                  apr21, may21)

# remove unwanted columns

trip <- trip %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng,
            start_station_id, end_station_id))

glimpse(trip)

# clean up and prepare data for analysis ------------------------------------

introduce(trip)
plot_intro(trip)
nrow(trip)
dim(trip)
summary(trip)

head(trip)


unique(trip$member_casual)
unique(trip$rideable_type)

trip <- trip %>% 
  rename(customer_type = member_casual)


trip <- trip %>% 
  mutate(start_station_name = as.factor(start_station_name),
         end_station_name = as.factor(end_station_name))

is.factor(trip$start_station_name)

# ride start time of the day
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
labels <- c("Night", "Morning", "Afternoon", "Evening")
trip$ride_start_time_of_day <- cut(x=hour(trip$started_at),
                        breaks = breaks, 
                        labels = labels, 
                        include.lowest=TRUE)

# ride length column
trip$ride_length <- difftime(trip$ended_at, trip$started_at,
                             units = "min")

# converting ride length to numeric
trip$ride_length <- as.numeric(as.character(trip$ride_length))
is.numeric(trip$ride_length)

# ride start day of the week

trip$date <- as.Date(trip$started_at)
# trip$month <- format(as.Date(trip$date), "%m")
# trip$day <- format(as.Date(trip$date), "%d")
# trip$year <- format(as.Date(trip$date), "%Y")
trip$day_of_week <- format(as.Date(trip$date), "%A")


# ride according to season
breaks <- month(ymd("2020-06-01", "2020-10-01",
                    "2020-12-01", "2020-03-01",
                    "2020-05-31"))

labels <- c("Spring", "Summer", "Fall", "Winter")
trip$season <- cut(x=month(trip$date),
                   breaks = breaks, 
                   labels = labels, 
                   include.lowest=TRUE)

  
  
  
glimpse(trip)
View(trip)

trip_v2 <- trip %>% 
  filter(ride_length > 0)
# 10787 records filtered. ride_length in minutes less than 0. 



View(trip_v2)

# Analyze Data --------------------------------------------------------------

# summary of ride length
trip_v2 %>% 
  summarise(mean(ride_length),
            median(ride_length),
            max(ride_length),
            min(ride_length))

# ride count for member and casual customer
trip_v2 %>% 
  count(customer_type)

# compare member and casual user
aggregate(trip_v2$ride_length ~ trip_v2$customer_type, FUN = mean)
aggregate(trip_v2$ride_length ~ trip_v2$customer_type, FUN = median)
aggregate(trip_v2$ride_length ~ trip_v2$customer_type, FUN = max)
aggregate(trip_v2$ride_length ~ trip_v2$customer_type, FUN = min)

# customer average ride length per day
trip_v2$day_of_week <- ordered(trip_v2$day_of_week, 
                               levels=c("Sunday", "Monday", "Tuesday", 
                                        "Wednesday", "Thursday", "Friday", 
                                        "Saturday"))


aggregate(trip_v2$ride_length ~ trip_v2$customer_type +
            trip_v2$day_of_week, FUN = mean)

# customer average ride length in different time of days
aggregate(trip_v2$ride_length ~ trip_v2$customer_type +
            trip_v2$ride_start_time_of_day, FUN = mean)

# customer average ride length per season
aggregate(trip_v2$ride_length ~ trip_v2$customer_type +
            trip_v2$season, FUN = mean)

# customer average ride length on different bikes
aggregate(trip_v2$ride_length ~ trip_v2$customer_type +
            trip_v2$rideable_type, FUN = mean)

# ride count for each kind of bikes
trip_v2 %>% 
  count(rideable_type, sort = TRUE)

# analyze ride count by customer type and weekday
trip_v2 %>% 
  group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, day_of_week)


# analyze ride count by customer type and time of the day
trip_v2 %>% 
  group_by(customer_type, ride_start_time_of_day) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, ride_start_time_of_day)

# analyze ride count by customer type and season
trip_v2 %>% 
  group_by(customer_type, season) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, season)

# analyze ride count by customer type and bike type
trip_v2 %>% 
  group_by(customer_type, rideable_type) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, rideable_type)

# analyze ride count by customer type, bike type and day of week
trip_v2 %>% 
  group_by(customer_type, rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, rideable_type, day_of_week)

# analyze ride count by customer type, bike type, day of week and season
trip_v2 %>% 
  drop_na() %>% 
  group_by(customer_type, rideable_type, season, day_of_week) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, rideable_type, season, day_of_week)

# analyze ride count by customer type and month
trip_v2 %>% 
  mutate(month = month(date, label = TRUE)) %>%
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, month)


# Most Popular Starting Station
trip_v2 %>% 
  drop_na() %>% 
  group_by(customer_type, start_station_name) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, start_station_name) %>% 
  slice_max(order_by = number_of_rides, n = 20)
  

# Most Popular End Station
trip_v2 %>% 
  drop_na() %>% 
  group_by(customer_type, end_station_name) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, end_station_name) %>% 
  slice_max(order_by = number_of_rides, n = 20)

# Casual customers riding habit by season and time of the day
trip_v2 %>% 
  filter(customer_type == 'casual') %>% 
  drop_na() %>% 
  group_by(season, ride_start_time_of_day) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(season, ride_start_time_of_day)

# regular members riding habit by season and time of the day
trip_v2 %>% 
  filter(customer_type == 'member') %>% 
  drop_na() %>% 
  group_by(season, ride_start_time_of_day) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(season, ride_start_time_of_day)



# Visualize ------------------------------------------------------------

# number of rides by customer type
trip_v2 %>% 
  group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, day_of_week) %>%
  ggplot(aes(day_of_week, number_of_rides, fill = customer_type)) + 
  geom_col(position = "dodge") +
  ggtitle("Number of Rides by Customer Type")
  
# average ride length by customer type
trip_v2 %>% 
  group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, day_of_week) %>%
  ggplot(aes(day_of_week, average_duration, fill = customer_type)) + 
  geom_col(position = "dodge") +
  ggtitle("Average Ride Length by Customer Type")


# visualize ride count by customer type and time of the day
trip_v2 %>% 
  group_by(customer_type, ride_start_time_of_day) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, ride_start_time_of_day) %>% 
  ggplot(aes(ride_start_time_of_day, number_of_rides, 
             fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle("Ride Count by Customer Type and Time of the day")


# visualize average ride length by customer type and time of the day
trip_v2 %>% 
  group_by(customer_type, ride_start_time_of_day) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, ride_start_time_of_day) %>% 
  ggplot(aes(ride_start_time_of_day,  average_duration, 
             fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle("Average Ride time by Customer Type and Time of the day")


# visualize average ride length by customer type and bike type
trip_v2 %>% 
  group_by(customer_type, rideable_type) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, rideable_type) %>% 
  ggplot(aes(rideable_type, average_duration, 
             fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle("Average Ride Time by Customer type and Bike Type")

# visualize ride count by customer type and bike type
trip_v2 %>% 
  group_by(customer_type, rideable_type) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, rideable_type) %>% 
  ggplot(aes(rideable_type, number_of_rides, 
             fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle("Ride Count by Customer Type and Bike Type")

# visualize ride count by customer type and season
trip_v2 %>% 
  drop_na() %>% 
  group_by(customer_type, season) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, season) %>% 
  ggplot(aes(season, number_of_rides, 
             fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle("Ride Count by Customer Type and Season")

# visualize average ride length by customer type and season
trip_v2 %>% 
  drop_na() %>% 
  group_by(customer_type, season) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, season) %>% 
  ggplot(aes(season, average_duration, 
             fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle("Average Ride Time by Customer Type and Season")

# visualize average ride length by customer type and month
trip_v2 %>% 
  mutate(month = month(date, label = TRUE)) %>%
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, month) %>%
  ggplot(aes(month, average_duration, fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle("Average Ride Time by Customer Type and Month")

# visualize ride count by customer type and month
trip_v2 %>% 
  mutate(month = month(date, label = TRUE)) %>%
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(customer_type, month) %>%
  ggplot(aes(month, number_of_rides, fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle("Average Ride Count by Customer Type and Month")

# Visualize Casual customers riding habit by season and time of the day
trip_v2 %>% 
  filter(customer_type == 'casual') %>% 
  drop_na() %>% 
  group_by(season, ride_start_time_of_day) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(season, ride_start_time_of_day) %>% 
  ggplot(aes(season, average_duration, 
             fill = ride_start_time_of_day)) +
  geom_col(position = "stack") +
  ggtitle("Casual Customer's Riding Habit by Season and Time of the Day")

# Visualize regular members riding habit by season and time of the day
trip_v2 %>% 
  filter(customer_type == 'member') %>% 
  drop_na() %>% 
  group_by(season, ride_start_time_of_day) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(season, ride_start_time_of_day) %>% 
  ggplot(aes(season, average_duration, 
             fill = ride_start_time_of_day)) +
  geom_col(position = "stack") +
  ggtitle("Regular Customer's Riding Habit by Season and Time of the Day")
