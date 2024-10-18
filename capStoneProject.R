library(readr)
library(tidyverse)
library(dplyr)

structureoOne <- list.files(path = "capStoneProjectData/structure1",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

structureTwo <- list.files(path = "capStoneProjectData/secondNewStructure",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

structureThree <- list.files(path = "capStoneProjectData/newStructure",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)
df <- readr::read_csv(structureoOne, col_types = cols(trip_id = col_character()))
df2 <- readr::read_csv(structureTwo, col_types = cols(trip_id = col_character()))
df3 <- readr::read_csv(structureThree)


# Cleaning & changing some column names and types
colnames(df)[which(names(df) == "starttime")] <- "start_time"
colnames(df)[which(names(df) == "stoptime")] <- "end_time"
colnames(df3)[which(names(df3) == "started_at")] <- "start_time"
colnames(df3)[which(names(df3) == "ended_at")] <- "end_time"
colnames(df3)[which(names(df3) == "member_casual")] <- "usertype"
colnames(df3)[which(names(df3) == "ride_id")] <- "trip_id"

# merging both dataframes and just getting the rows which tripduration is greater than 1
result <- rbind(df %>% filter(tripduration > 1), df2 %>% filter(tripduration > 1))

# rename other columns
colnames(result)[which(names(result) == "from_station_id")] <- "start_station_id" # nolint: line_length_linter.
colnames(result)[which(names(result) == "from_station_name")] <- "start_station_name" # nolint: line_length_linter.
colnames(result)[which(names(result) == "to_station_id")] <- "end_station_id"
colnames(result)[which(names(result) == "to_station_name")] <- "end_station_name" # nolint: line_length_linter.

# remove unnecesary columns
result <- within(result, rm(gender, birthyear, bikeid))
df3 <- within(df3, rm(end_lng, end_lat, start_lat, start_lng))

# adding column trip duration to the newStructure of data from 2020> to cpmply with the legacy data # nolint: line_length_linter.
#TODO: NEED TO CHECK THIS, THERE IS AN ERROR
df3 <- df3 %>% mutate(tripduration = as.numeric(difftime(end_time, start_time, units="secs")))

# change column types to match each other
result$start_station_id <- as.character(result$start_station_id)
result$end_station_id <- as.character(result$end_station_id)
# will use GMT
result$start_time <- as.POSIXct(result$start_time,format = "%Y-%m-%d %H:%M", tz = "GMT")
result$end_time <- as.POSIXct(result$end_time,format = "%Y-%m-%d %H:%M", tz = "GMT")

# binding the 2 dataframes and just getting the rows which tripduration is greater than 1 from df3
all_data <- bind_rows(result, df3 %>% filter(tripduration > 1))



str(all_data)

# ANALYSIS
all_data %>%
    filter(tripduration > 1) %>%
    group_by(usertype) %>%
    summarise(average_trip_duration=mean(tripduration),
            max_trip_duration=max(tripduration),
            min_trip_duration=min(tripduration),
            total_trips = n())