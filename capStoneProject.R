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

# ANALYSIS
# average trip duration and totaltrips per usertype and per weekday
all_data %>% 
    group_by(usertype,start_week_day) %>%
    summarise(average_trip_duration=mean(tripduration),
            total_trips = n()) %>%
            arrange(usertype, start_week_day)

# average trip duration per usertype and weekday in pivot table
all_data %>% 
    group_by(usertype,start_week_day) %>%
    summarise(average_trip_duration=mean(tripduration)) %>%
    pivot_wider(names_from = start_week_day, values_from = average_trip_duration)

# totaltrips per usertype and weekday in pivot table
all_data %>% 
    group_by(usertype,start_week_day) %>%
    summarise(total_trips=n()) %>%
    pivot_wider(names_from = start_week_day, values_from = total_trips)

# comparison casual vrs member on average trip duration per year per month
all_data %>% 
    group_by(usertype, start_year, start_month) %>%
    summarise(average_trip_duration=mean(tripduration)) %>%
    pivot_wider(names_from = c(start_year, start_month), values_from = average_trip_duration)

#Graph
all_data %>% 
    group_by(usertype, start_year, start_month) %>%
    summarise(average_trip_duration=mean(tripduration)) %>%
    ggplot(aes(x = start_year, y = average_trip_duration, fill = start_month)) +
    geom_bar(stat='identity') +
    facet_wrap(~usertype)

# comparison casual vrs member on totalTrips per year from 2018 to 2024
all_data %>% 
    group_by(usertype,start_year) %>%
    summarise(total_trips = n()) %>%
    pivot_wider(names_from = start_year, values_from = total_trips)

#Graph
all_data %>% 
    group_by(usertype,start_year) %>%
    summarise(total_trips = n()) %>%
ggplot(aes(x = start_year, y = total_trips)) +
    geom_bar(stat='identity') +
    facet_wrap(~usertype)

# comparison casual vrs member on totalTrips per year per month
all_data %>% 
    group_by(usertype,start_year,start_month) %>%
    summarise(total_trips = n()) %>%
    pivot_wider(names_from = c(start_year,start_month), values_from = total_trips)