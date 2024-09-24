# Cyclistic Bikeshare
Google Data Analytic Capstone Cyclist Bikeshare (R Studio)

install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("hms")

library(tidyverse)
library(dplyr)
library(lubridate)
library(hms)

trip_sep23 <- read.csv("202309-divvy-tripdata.csv")
trip_oct23 <- read.csv("202310-divvy-tripdata.csv")
trip_nov23 <- read.csv("202311-divvy-tripdata.csv")
trip_dec23 <- read.csv("202312-divvy-tripdata.csv")
trip_jan24 <- read.csv("202401-divvy-tripdata.csv")
trip_feb24 <- read.csv("202402-divvy-tripdata.csv")
trip_mar24 <- read.csv("202403-divvy-tripdata.csv")
trip_apr24 <- read.csv("202404-divvy-tripdata.csv")
trip_may24 <- read.csv("202405-divvy-tripdata.csv")
trip_jun24 <- read.csv("202406-divvy-tripdata.csv")
trip_jul24 <- read.csv("202407-divvy-tripdata.csv")
trip_aug24 <- read.csv("202408-divvy-tripdata.csv")

colnames(df_original)
str(df_original)

num_rows <- nrow(df_original)
print(num_rows)

df_original$started_at <- as.POSIXct(df_original$started_at)
df_original$ended_at <- as.POSIXct(df_original$ended_at)
class(df_original$started_at) 
class(df_original$ended_at)

df_original$ride_length <- difftime(df_original$ended_at, 
df_original$started_at, units = "mins")
class(df_original$ride_length)

df_original$ride_length <- as.numeric(as.character(df_original$ride_length))
is.numeric(df_original$ride_length)

df_original$day_of_week <- wday(df_original$started_at)

write.csv(df_original, file = "df_original.csv", row.names = FALSE)

df_updated <- df_original
View(df_updated)

df_updated <- distinct(df_updated)
df <- na.omit(df_original)
df_updated <- df_updated[!(df_updated$ride_length<=0),]

df_updated <- df_updated %>% 
  rename(membership = member_casual)

write.csv(df_updated, file = "df_updated.csv", row.names = FALSE)

df_analyze <- df_updated[, c("ride_id", "rideable_type", "membership", 
"ride_length", "day_of_week")]
View(df_analyze)

write.csv(df_analyze, file = "df_analyze.csv", row.names = FALSE)

total_rides <- length(df1$ride_id)
print(total_rides)

mean_ride_length <- mean(df_analyze$ride_length)
print(mean_ride_length)

max_ride_length <- max(df_analyze$ride_length)
print(max_ride_length)

min_ride_length <- min(df_analyze$ride_length)
print(min_ride_length)

day_counts <- table(df1$day_of_week)
print(day_counts)


sum_member <-sum(df_analyze$membership == "member")
print(sum_member)

sum_casual <-sum(df_analyze$membership == "casual")
print(sum_casual)

casual_membership <- df_analyze %>% 
  filter(membership == "casual")
View(casual_membership)
write.csv(casual_membership, file = "casual_membership.csv", row.names = FALSE)

average_ride_length <- df_analyze %>%
  group_by(membership) %>%
  summarize(average_ride_length = mean(ride_length))

df_analyze %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

df_analyze %>% 
  group_by(rideable_type, membership) %>% 
  dplyr::summarize(count_trips = n()) %>% 
  ggplot(aes(x = rideable_type, y = count_trips, fill = membership, 
  color = membership)) + geom_bar(stat = 'identity', position = 'dodge') + 
  theme_classic() + labs(title = "Trips Count By Bike Type", x = "Bike Type", 
  y = "Trips Count")

df_analyze <- df_analyze %>% 
  mutate(day_of_week = recode (day_of_week, "1" = "Mon", "2" = "Tue", 
  "3" = "Wed", "4" = "Thu", "5" = "Fri", "6" = "Sat", "7" = "Sun"))
View(df_analyze)

df_analyze %>% 
  group_by(membership, day_of_week) %>%
    dplyr::summarize(count_trips = n()) %>%  
      ggplot(aes(x= day_of_week, y=count_trips, fill=membership, 
      color=membership)) + geom_bar(stat='identity', position = 'dodge') 
      + theme_classic()+ labs(title ="Total Rides per Day", 
      x = "Day of a Week", y = "Total Rides")
