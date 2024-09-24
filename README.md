# Cyclistic Bikeshare
Google Data Analytic Capstone Cyclist Bikeshare (R)

# Preparation

First thing first, we need to install the packages and open the library before we load the data.

>> install.packages("tidyverse")
>> install.packages("dplyr")
>> install.packages("lubridate")
>> install.packages("hms")

>> library(tidyverse)
>> library(dplyr)
>> library(lubridate)
>> library(hms)

Then we load the data from the database given. In this case, i would like to use the latest 12 months data from September 2023 to August 2024.
>> trip_sep23 <- read.csv("202309-divvy-tripdata.csv")
>> trip_oct23 <- read.csv("202310-divvy-tripdata.csv")
>> trip_nov23 <- read.csv("202311-divvy-tripdata.csv")
>> trip_dec23 <- read.csv("202312-divvy-tripdata.csv")
>> trip_jan23 <- read.csv("202401-divvy-tripdata.csv")
>> trip_feb23 <- read.csv("202402-divvy-tripdata.csv")
>> trip_mar23 <- read.csv("202403-divvy-tripdata.csv")
>> trip_apr23 <- read.csv("202404-divvy-tripdata.csv")
>> trip_may23 <- read.csv("202405-divvy-tripdata.csv")
>> trip_jun23 <- read.csv("202406-divvy-tripdata.csv")
>> trip_jul23 <- read.csv("202407-divvy-tripdata.csv")
>> trip_aug23 <- read.csv("202408-divvy-tripdata.csv")

>> colnames(df_original)
>> str(df_original)

# Print total rows on "df_original" table

>> num_rows <- nrow(df_original)
>> print(num_rows)

---------------------------------------
| NO |     Attribute      | Date Type |
|-------------------------------------|
| 1. |ride_id             | chr       |
| 2. |readable_type       | chr       |       
| 3. |started_at          | chr       |
| 4. |ended_at            | chr       |       
| 5. |start_station_name  | chr       |
| 6. |start_station_id    | chr       |
| 7. |end_station_name    | chr       |
| 8. |end_station_id      | chr       |
| 9. |start_lat           | num       |
|10. |start_lng           | num       |
|11. |end_lat             | num       |
|12. |end_lng             | num       |
|13. |member_casual       | chr       |
---------------------------------------

# Change data type from "chr" to "POSIXct"

>> df_original$started_at <- as.POSIXct(df_original$started_at)
>> df_original$ended_at <- as.POSIXct(df_original$ended_at)
>> class(df_original$started_at) 
>> class(df_original$ended_at)

# Calculate ride length and store it into new column "ride_length".

df_original$ride_length <- difftime(df_original$ended_at, 
df_original$started_at, units = "mins")
class(df_original$ride_length)

# Change "ride_length" data type from "difftime"  to "num"

df_original$ride_length <- as.numeric(as.character(df_original$ride_length))
is.numeric(df_original$ride_length)

# create column "day_of_week"

df_original$day_of_week <- wday(df_original$started_at)

#save "df_original.csv" table
write.csv(df_original, file = "df_original.csv", row.names = FALSE)

# Duplicate the original table to "df_updated" table
df_updated <- df_original
View(df_updated)

# remove duplicate rows
df_updated <- distinct(df_updated)

# remove rows with null values
df <- na.omit(df_original)

# remove rows where ride_length is <= zero
df_updated <- df_updated[!(df_updated$ride_length<=0),]

# rename member_casual to membership
df_updated <- df_updated %>% 
  rename(membership = member_casual)

# save df_updated.csv table
write.csv(df_updated, file = "df_updated.csv", row.names = FALSE)

# Choose only needed column

df_analyze <- df_updated[, c("ride_id", "rideable_type", "membership", 
"ride_length", "day_of_week")]
View(df_analyze)

#save df_analyze.csv table
write.csv(df_analyze, file = "df_analyze.csv", row.names = FALSE)

# calculate Total Rides on df_analyze data frame
total_rides <- length(df1$ride_id)
print(total_rides)

# calculate mean (average) of ride_length
mean_ride_length <- mean(df_analyze$ride_length)
print(mean_ride_length)

# calculate max ride_length
max_ride_length <- max(df_analyze$ride_length)
print(max_ride_length)

# calculate min ride_length
min_ride_length <- min(df_analyze$ride_length)
print(min_ride_length)

# calculate mode day_of_week
day_counts <- table(df1$day_of_week)
print(day_counts)

Total Rides         = 5,690,850 rides

Mean of ride_length = 15.59464 min

Max of ride_length  = 1500.517 min

Min of ride_length  = 0.0006499966 min

Mode of day_of_week =

-----------------------
| NO | Var |   Rides  |
|---------------------|
| 1. |  1  | 754949   |
| 2. |  2  | 734577   |       
| 3. |  3  | 779300   |
| 4. |  4  | 851673   |       
| 5. |  5  | 822275   |
| 6. |  6  | 828232   |
| 7. |  7  | 919844   |
-----------------------

# Calculate total rides who has "member" type of membership
sum_member <-sum(df_analyze$membership == "member")
print(sum_member)

# Calculate total rides who has "casual" type of membership
sum_casual <-sum(df_analyze$membership == "casual")
print(sum_casual)

# Filter only "Casual" membership data and save table as csv. file
casual_membership <- df_analyze %>% 
  filter(membership == "casual")
View(casual_membership)
write.csv(casual_membership, file = "casual_membership.csv", row.names = FALSE)

Total rides of "member" membership   = 3,651,580 rides
Total rides of "casual" membership   = 2,039,270 rides

# Average ride_length for casual and member riders
average_ride_length <- df_analyze %>%
  group_by(membership) %>%
  summarize(average_ride_length = mean(ride_length))

# Total riders for each bike type
df_analyze %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

# Here is the ride length average of each membership
Average ride length of "member" membership   = 21.5 min
Average ride length of "casual" membership   = 12.3 min

# Here is the total riders for each bike types
1. classic_bike : 2.810.606 riders
2. electric_bike: 2.880.244 riders



