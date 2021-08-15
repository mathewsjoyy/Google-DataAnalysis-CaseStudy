# Install required packages
# tidyverse for data import and wrangling
# libridate for date functions
# ggplot for visualization
# data.table for certain functions
# extrafont for changing fonts of plots

# Install packages
install.packages(c("tidyverse", "lubridate", "ggplot2","data.table","ggthemes"))
# Load in packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(data.table)
library(ggthemes)


getwd() # Displays current working directory
setwd("/Users/mathe/OneDrive/Documents/Data analysis/R/Google-DataAnalysis-CaseStudy") # Set our working directory for this project

# Importing the 12 data sets into R
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
# (they don't need to be in the exact order however)
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
# Can use the mutate function to change multiple column datatypes
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
all_bike_trips$ride_length <- (as.double(difftime(all_bike_trips$end_time,
                                                  all_bike_trips$start_time))) / 60

glimpse(all_bike_trips)


# Check for any negatives (invalid values) to prevent later conflicts
sum(all_bike_trips$ride_length < 0)

# We have been informed that the company had "test" stations used for quality checks
# So we can see how many there are, we don't need to check end station name, as start
# and end are going to be on the same row anyway so it doesn't matter when we come to delete
# the rows
sum(all_bike_trips$start_station_name %like% "test" +
      all_bike_trips$start_station_name %like% "TEST" +
      all_bike_trips$start_station_name %like% "Test")

# So we have gathered there are 3367 tests, and 10552 negative values, so we can
# remove these from the data-set as they can cause issues in the future. We should
# create a new data frame for this as we are removing data

# remove negative trip durations 
all_bike_trips_v2 <- all_bike_trips[!(all_bike_trips$ride_length < 0),]

#remove test rides
all_bike_trips_v2<- all_bike_trips_v2[!((all_bike_trips_v2$start_station_name %like% "TEST" |
                                 all_bike_trips_v2$start_station_name %like% "test" |
                                 all_bike_trips_v2$start_station_name %like% "Test")),]

# We can also drop columns we don't need for our analysis such as latitude and
# longitude values
all_bike_trips_v2 <- all_bike_trips_v2 %>% select(-c(start_lat:end_lng))

glimpse(all_bike_trips_v2)


# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year.
# Before completing these operations we could only aggregate at the ride level

all_bike_trips_v2$date <- as.Date(all_bike_trips_v2$start_time) # The default is yyyy/mm/dd
all_bike_trips_v2$month <- format(as.Date(all_bike_trips_v2$date), "%b_%Y") #May_2020, Jul_2020
all_bike_trips_v2$day <- format(as.Date(all_bike_trips_v2$date), "%d") #dd
all_bike_trips_v2$year <- format(as.Date(all_bike_trips_v2$date), "%Y") #yyyy
all_bike_trips_v2$day_of_week <- format(as.Date(all_bike_trips_v2$date), "%a") #Sun, Mon


# Check our data set for NA (null) values
sum(is.na(all_bike_trips_v2) == FALSE) # Returns sum of non NA values
sum(is.na(all_bike_trips_v2) == TRUE) # Returns sum of NA values
# 1057208 (total NA values) is 2.03 % of 52132260 (total values)
summary(all_bike_trips_v2)

# We have gathered all the null values are in start_station_id and end_station_id
# which are not important columns for us when we analyse, however in order to prevent
# data bias we will keep the NA rows as the more important data is still present
# Another option is to match name and id, by comparing to existing data, however
# for our scope of this project it isn't necessary.



# Run a few calculations to gain more insight into the data
# Mean,min,max,medium of ride_length, mode of day_of_week 
summary(all_trips_v2$trip_duration) # mean,min,max,medium

# (R does not have a standard in-built function to calculate mode.)
# Users defined mode function credit (https://www.tutorialspoint.com)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(all_bike_trips_v2$day_of_week) # Saturday is most common day

# In depth table summary for customer types
all_bike_trips_v2 %>% group_by(type_of_member) %>%
  summarise(min_trip_duration = min(ride_length), max_trip_duration = max(ride_length),
            mean_trip_duration = mean(ride_length), mode_week_day = getmode(day_of_week),
            most_popular_bike_type = getmode(type_of_ride))

# See the average ride time by each day for members vs casual users
aggregate(all_bike_trips_v2$ride_length ~ all_bike_trips_v2$type_of_member +
            all_bike_trips_v2$day_of_week, FUN = mean)
# Notice that the days of the week are out of order. Let's fix that.
all_bike_trips_v2$day_of_week <- ordered(all_bike_trips_v2$day_of_week, 
                                    levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
# We can then call the aggregate function above again...
aggregate(all_bike_trips_v2$ride_length ~ all_bike_trips_v2$type_of_member +
            all_bike_trips_v2$day_of_week, FUN = mean)

# We can also fix order of month column while we are at it
all_bike_trips_v2$month <- ordered(all_bike_trips_v2$month,
                                   levels=c("Apr_2020", "May_2020", "Jun_2020", "Jul_2020",
                                   "Aug_2020", "Sep_2020", "Oct_2020", "Nov_2020", "Dec_2020",
                                   "Jan_2021", "Feb_2021", "Mar_2021"))

# Analyze ridership data by type and day of week
all_bike_trips_v2 %>% group_by(type_of_member, day_of_week) %>%
              summarise(num_of_rides = n(), avg_ride_length_mins = mean(ride_length)) %>%
              arrange(type_of_member, desc(num_of_rides))


# Lets visualize the data we gathered from above
all_bike_trips_v2 %>% group_by(type_of_member, day_of_week) %>%
  summarise(num_of_rides = n(), avg_ride_length_mins = mean(ride_length)) %>%
  arrange(type_of_member, desc(num_of_rides)) %>%
  ggplot(aes(x=num_of_rides ,y=day_of_week, fill=type_of_member)) +
  labs(title="Day of the week VS Number of rides", subtitle="Sample of 2 customer types (casual or member)",
       x="Number Of Rides", y="Day Of The Week", fill="Member Type")+
  geom_col(width=0.4, position = "dodge", orientation = "y") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +# We want to scale x axis to no have scientific notation
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) 
  # When adding a plot theme is removing axis labels so we re add them 'axis.title = element_text()'

# Export vis (placed into visualization folder)
ggsave("visualizations/bike_viz_1.png")








