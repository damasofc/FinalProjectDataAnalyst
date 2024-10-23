library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

structureTwo <- list.files(path = "capStoneProjectData/secondNewStructure",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

structureThree <- list.files(path = "capStoneProjectData/newStructure",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)                           
# on df and df2 whne reading we change the column type for trip_id to be a char
df2 <- read_csv(structureTwo, col_types = cols(trip_id = col_character()))
df3 <- read_csv(structureThree)
# Changing some column names to be consistent
df3 <- rename(df3,
        start_time = started_at,
        end_time = ended_at,
        usertype = member_casual,
        trip_id = ride_id)

# just getting the rows which tripduration is greater than 1
# and renaming columns
df2 <- df2 %>% filter(tripduration > 1)
df2 <- rename(df2,
        start_station_id = from_station_id,
        start_station_name = from_station_name,
        end_station_id = to_station_id,
        end_station_name = to_station_name)

# remove unnecesary columns
df2 <- within(df2, rm(gender, birthyear, bikeid))
df3 <- within(df3, rm(end_lng, end_lat, start_lat, start_lng))
# change column types to match each other
df2$start_station_id <- as.character(df2$start_station_id)
df2$end_station_id <- as.character(df2$end_station_id)
df2$start_time <- as.POSIXct(df2$start_time,format = "%Y-%m-%d %H:%M", tz = "GMT")
df2$end_time <- as.POSIXct(df2$end_time,format = "%Y-%m-%d %H:%M", tz = "GMT")


# adding column trip duration to the newStructure of data from 2020> to cpmply with the legacy data # nolint: line_length_linter.
df3 <- df3 %>% mutate(tripduration = as.numeric(difftime(end_time, start_time, units="secs")))

# binding the 2 missing dataframes and just getting the rows which tripduration is greater than 1 from df3
all_data <- bind_rows(df2, df3 %>% filter(tripduration > 1))

# change to the right values on usertype and remove rows which are empty on start_time
all_data <- all_data %>% 
    mutate(start_week_day = wday(start_time,label=TRUE),
    start_month = month(start_time,label=TRUE),
    start_year = year(start_time)) %>%
    mutate(usertype = recode(usertype
                            ,"Subscriber" = "member"
                            ,"Customer" = "casual")) %>%
                            filter(usertype != "Dependent", !is.na(start_time), start_station_name != "HQ QR")

# START ANALYSIS

#Table - average trip duration and totaltrips per usertype and per weekday
all_data %>% 
    group_by(usertype,start_week_day) %>%
    summarise(average_trip_duration=mean(tripduration),
            total_trips = n()) %>%
            arrange(usertype, start_week_day)
#Table - average trip duration per usertype and weekday in pivot table
all_data %>% 
    group_by(usertype,start_week_day) %>%
    summarise(average_trip_duration=mean(tripduration)) %>%
    pivot_wider(names_from = start_week_day, values_from = average_trip_duration)

#Table - comparison casual vrs member on average trip duration per year per month
all_data %>% 
    group_by(usertype, start_year, start_month) %>%
    summarise(average_trip_duration=mean(tripduration)) %>%
    pivot_wider(names_from = c(start_year, start_month), values_from = average_trip_duration)

#Table - totaltrips per usertype and weekday in pivot table
all_data %>% 
    group_by(usertype,start_week_day) %>%
    summarise(total_trips=n()) %>%
    pivot_wider(names_from = start_week_day, values_from = total_trips)

#Table - comparison casual vrs member on totalTrips per year from 2018 to 2024 pero usertype
all_data %>% 
    group_by(usertype,start_year) %>%
    summarise(total_trips = n()) %>%
    pivot_wider(names_from = start_year, values_from = total_trips)

#Table - comparison casual vrs member on totalTrips per year per month
all_data %>% 
    group_by(usertype,start_year,start_month) %>%
    summarise(total_trips = n()) %>%
    pivot_wider(names_from = c(start_year,start_month), values_from = total_trips)

# END ANALYSIS


#START GRAPHS

#1 Graph Column: Average Trip Duration per usertype and Weekday**
all_data %>%
    group_by(usertype, start_week_day) %>%
    summarise(average_trip_duration=mean(tripduration)) %>%
    arrange(usertype, start_week_day) %>%
    ggplot(aes(x = start_week_day, y = average_trip_duration, fill = usertype)) +
    geom_col(position = "dodge") +
    labs(title="Average Trip Duration per usertype and Weekday")

#2 Graph Column: Average Trip duration  per UserType and year**
all_data %>%
    group_by(usertype, start_year) %>%
    summarise(average_trip_duration=mean(tripduration)) %>%
    ggplot(aes(x = start_year, y = average_trip_duration, fill = usertype)) +
    geom_col(position = "dodge") +
    labs(title="Average Trip Duration per usertype and year")

#3 Graph Column: Average Trip duration per UserType, month and year
all_data %>% 
    group_by(usertype, start_year, start_month) %>%
    summarise(average_trip_duration=mean(tripduration)) %>%
    ggplot(aes(x = start_month, y = average_trip_duration, fill = usertype)) +
    geom_col(position = "dodge") +
    facet_wrap(~start_year) +
    labs(title="Average Trip Duration per usertype, month and year")

#4 Graph Column: Number of trips  per UserType and week day**
all_data %>%
    group_by(usertype, start_week_day) %>%
    summarise(total_trips = n()) %>%
    arrange(usertype, start_week_day) %>%
    ggplot(aes(x = start_week_day, y = total_trips, fill = usertype)) +
    geom_col(position = "dodge") +
    labs(title="Number of trips  per UserType and week day")

#5 Graph Column: Number of trips per UserType and year
all_data %>%
    group_by(usertype, start_year) %>%
    summarise(number_of_rides = n()) %>%
    ggplot(aes(x = start_year, y = number_of_rides, fill = usertype)) +
    geom_col(position = "dodge") +
    labs(title="Number of trips per UserType and year")

#6 Graph Line: Number of trips  per UserType, month and year**
all_data %>% 
    group_by(usertype,start_year,start_month) %>%
    summarise(total_trips = n()) %>%
    ggplot(aes(x = start_month, y = total_trips, group = 1)) +
    geom_line(aes(color=usertype, group=usertype)) +
    facet_wrap(~start_year) +
    labs(title="Number of trips per UserType, month and year")

ggsave("numberTrips_month_year.png", width = 12, height = 10)