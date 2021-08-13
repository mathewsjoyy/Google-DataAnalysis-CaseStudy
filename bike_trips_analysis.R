# Install required packages
# tidyverse for data import and wrangling
# libridate for date functions
# ggplot for visualization

# Install packages
install.packages(c("tidyverse", "lubridate", "ggplot2"))
# Load in packages
library(tidyverse)
library(lubridate)
library(ggplot2)

getwd() # Displays current working directory
setwd("/Users/mathe/OneDrive/Documents/Data analysis/R/Google-DataAnalysis-CaseStudy") # Set our working directory for this project


# Importing the 12 datasets into R
apr20 <- read_csv("data_sets/202004-divvy-tripdata.csv")
may20 <- read_csv("data_sets/202005-divvy-tripdata.csv")
jun20 <- read_csv("data_sets/202006-divvy-tripdata.csv")
jul20 <- read_csv("data_sets/202007-divvy-tripdata.csv")
aug20 <- read_csv("data_sets/202008-divvy-tripdata.csv")
sep20 <- read_csv("data_sets/202009-divvy-tripdata.csv")
oct20 <- read_csv("data_sets/202010-divvy-tripdata.csv")
nov20 <- read_csv("data_sets/202011-divvy-tripdata.csv")
dec20 <- read_csv("data_sets/202012-divvy-tripdata.csv")
jan21 <- read_csv("data_sets/202101-divvy-tripdata.csv")
feb21 <- read_csv("data_sets/202102-divvy-tripdata.csv")
mar21 <- read_csv("data_sets/202103-divvy-tripdata.csv")

# Check all columns names are the exact same, so that we are be able to join them all
# (they dont need to be in the exact order however)
colnames(apr20)
colnames(may20)
colnames(jun20)
colnames(jul20)
colnames(aug20)
colnames(sep20)
colnames(oct20)
colnames(nov20)
colnames(dec20)
colnames(jan21)
colnames(feb21)
colnames(mar21)

# Check datatypes of columns in datasets for consistency
sapply(apr20, typeof)
sapply(may20, typeof)
sapply(jun20, typeof)
sapply(jul20, typeof)
sapply(aug20, typeof)
sapply(sep20, typeof)
sapply(oct20, typeof)
sapply(nov20, typeof)
sapply(dec20, typeof)
sapply(jan21, typeof)
sapply(feb21, typeof)
sapply(mar21, typeof)

# We can see start_station_id and end_station_id are char in dec20 - mar21 (should be double)
# Can use the mutate function to change mutiple column datatypes
dec20 <- mutate(dec20, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))
jan21 <- mutate(jan21, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))
feb21 <- mutate(feb21, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))
mar21 <- mutate(mar21, start_station_id = as.double(start_station_id),
                end_station_id = as.double(end_station_id))

# Check the changes were applied correctly
is.double(dec20$start_station_id)
is.double(dec20$end_station_id)
is.double(jan21$start_station_id)
is.double(jan21$end_station_id)
is.double(feb21$start_station_id)
is.double(feb21$end_station_id)
is.double(mar21$start_station_id)
is.double(mar21$end_station_id)


# We can now combine all the cleaned datasets into one 
# We cannot use the common rbind() function as it changes the datetime format
# of the started_at and ended_at columns to numeric, so we use bind_rows() instead 
all_bike_trips <- bind_rows(apr20,may20,jun20,jul20,aug20,sep20,oct20,nov20,
                         dec20,jan21,feb21,mar21)

# View/inspect the combined data frame
str(all_bike_trips)
summary(all_bike_trips) # Get a quick summary
dim(all_bike_trips) # Have a look at the dimensions

# Make sure we only have 2 member types (casual and member)
table(all_bike_trips$member_casual)

# Rename some column names for better readability
all_bike_trips <- all_bike_trips %>% rename(
  type_of_ride = rideable_type, 
  start_time = started_at, 
  end_time = ended_at,
  type_of_member = member_casual
)

str(all_bike_trips)


# Add a column called ride length to find total trip duration in minutes
# Find the trip / ride duration by finding diff between ended and started
all_bike_trips$ride_length <- (as.double(difftime(all_bike_trips$end_time, all_bike_trips$start_time))) / 60

glimpse(all_bike_trips)















