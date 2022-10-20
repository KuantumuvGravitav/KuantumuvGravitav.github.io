library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(knitr)
library(readr)
library(rmarkdown)
library(janitor)
library(skimr)
library(tibble)
library(yaml)
library(viridis)

# loading csv and cleaning the dataset
daily_activity <- read_csv("C:/Users/user/Documents/R Programming/My Data Analytics Case Studies/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv") %>%
  clean_names()

daily_steps <- read_csv("C:/Users/user/Documents/R Programming/My Data Analytics Case Studies/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv") %>% 
  clean_names()

daily_sleep <- read_csv("C:/Users/user/Documents/R Programming/My Data Analytics Case Studies/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv") %>% 
  clean_names()

hourly_intensities <- read_csv("C:/Users/user/Documents/R Programming/My Data Analytics Case Studies/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv") %>% 
  clean_names()

# exploring the data structures
glimpse(daily_activity)
glimpse(daily_steps)
glimpse(daily_sleep)
glimpse(hourly_intensities)

# Summary statistics
daily_activity %>%
  select(total_steps, total_distance, calories) %>%
  summary()

daily_steps %>%
  select(step_total) %>%
  summary()

daily_sleep %>%
  select(total_sleep_records, total_minutes_asleep, total_time_in_bed) %>%
  summary()

hourly_intensities %>%
  select(total_intensity, average_intensity) %>%
  summary()

# Counting ids
n_distinct(daily_activity$id) #33
n_distinct(daily_steps$id) #33
n_distinct(daily_sleep$id) #24
n_distinct(hourly_intensities$id) #33

# Checking for missing values
sum(is.na(daily_activity)) # 0
sum(is.na(daily_steps)) # 0
sum(is.na(daily_sleep)) # 0
sum(is.na(hourly_intensities)) # 0

sum(duplicated(daily_activity)) # No duplicates
sum(duplicated(daily_steps)) # No duplicates
sum(duplicated(daily_sleep)) # 3 duplicates
sum(duplicated(hourly_intensities)) # No

# Removing duplicates
daily_sleep <- daily_sleep %>%
  distinct()
# Check duplicates
sum(duplicated(daily_sleep))

# Matching columns for later combination and creating day_of_the_week column
daily_activity_modified <- select(daily_activity, id, activity_date, total_steps, total_distance, tracker_distance, logged_activities_distance, very_active_minutes, fairly_active_minutes, lightly_active_minutes, calories, sedentary_minutes) %>%
  rename(date = activity_date) %>%
  mutate(date = mdy(date)) %>%
  mutate(day_of_the_week = weekdays(date)) %>%
  rename(total_distance_km = total_distance) %>%
  rename(tracker_distance_km = tracker_distance) %>%
  rename(logged_distance_km = logged_activities_distance) %>%
  rename(very_active_mins = very_active_minutes) %>%
  rename(fairly_active_mins = fairly_active_minutes) %>%
  rename(sedentary_mins = sedentary_minutes) %>%
  rename(lightly_active_mins = lightly_active_minutes)

daily_sleep_modified <- select(daily_sleep, id, sleep_day, total_minutes_asleep, total_time_in_bed) %>%
  rename(date = sleep_day) %>%
  separate(date, into = c('date', 'hour', 'AM_PM'), sep = ' ') %>%
  mutate(date = mdy(date)) %>%
  mutate(day_of_the_week = weekdays(date)) %>%
  rename(time_asleep_mins = total_minutes_asleep) %>%
  rename(time_in_bed_mins = total_time_in_bed)

hourly_intensities_modified <- select(hourly_intensities, id, activity_hour, total_intensity, average_intensity) %>%
  rename(date = activity_hour) %>%
  separate(date, into = c('date', 'hour', 'AM_PM'), sep = ' ') %>%
  mutate(date = mdy(date)) %>%
  mutate(day_of_the_week = weekdays(date))

glimpse(daily_activity_modified)
glimpse(daily_sleep_modified)
glimpse(hourly_intensities_modified)

# Combine the compatible tables
daily_data_combined <- left_join(daily_activity_modified, daily_sleep_modified)
glimpse(daily_data_combined)

daily_data_completed <- left_join(daily_data_combined, hourly_intensities_modified)
glimpse(daily_data_completed)

n_distinct(daily_data_completed$id)
sum(is.na(daily_data_completed))

daily_data_completed <- daily_data_completed %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

sum(is.na(daily_data_completed))
glimpse(daily_data_completed)
n_distinct(daily_data_completed$id)
head(daily_data_completed)

daily_data_completed$day_of_the_week <- factor(daily_data_completed$day_of_the_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
head(daily_data_completed)

# 4. ANALYZE PHASE

# Number of log-in times per user weekly
ggplot(data=daily_data_completed, aes(x=day_of_the_week, fill=day_of_the_week)) +
  geom_bar(stat='count', color='black') +
  theme(plot.title = element_text(hjust=1, lineheight=1, face='bold'),
        axis.text.x = element_text(angle=90), axis.text.y = element_text(angle = 90)) + facet_wrap(~id) +
  labs(x='Day of the Week', y='Frequency',
       title='Figure 1. Number of log-in times per user weekly')

# Distance vs. Calories
daily_data_completed %>%  
  select(total_distance_km, calories) %>%
  summary()

ggplot(data=daily_data_completed,
       mapping=aes(x = total_distance_km, y = calories,
                   color = total_intensity)) +
  geom_point() + geom_smooth() +
  scale_color_gradientn(colours = topo.colors(10)) +
  geom_hline(yintercept = 2304, color = "black", size = 1) +
  geom_vline(xintercept = 5.490, color = "black", size = 1) +
  geom_text(aes(x=8, y=2500, label="Mean"), color="black", size=4) +
  theme(plot.title = element_text(hjust = 0.5,
                                  lineheight = 0.8, face = "bold")) +
  labs(x = 'Total Distance (km)', y = 'Calories Burned',
       title = 'Figure 2. Calories burned per distance kilometer')

ggplot(data=daily_data_completed,
       mapping=aes(x = calories, y = total_steps, color = total_distance_km)) +
  geom_point() + geom_smooth() +
  scale_color_gradientn(colours = topo.colors(10)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  lineheight = 0.8, face = "bold")) +
  labs(x = 'Calories', y = 'Total Steps',
       title = 'Figure 3. Calories burned per step')

# National Sleep Foundation guidelines1 advise that healthy adults need between 7 and 9 hours of sleep per night.
daily_data_completed <- daily_data_completed %>%
  mutate(sleep_duration = case_when(time_asleep_mins == 0 ~ 'did not record sleep',
                                time_asleep_mins < 420 & time_asleep_mins > 0 ~ 'less than 7 hours',
                                time_asleep_mins >= 420 & time_asleep_mins <= 540 ~ '7 to 9 hours',
                                time_asleep_mins > 540 ~ 'more than 9 hours'))
# Check if correct
head(daily_data_completed[c('time_asleep_mins', 'sleep_duration')], 6)
summary(daily_data_completed['time_asleep_mins'], na.rm = FALSE)

# sleep distribution plot
daily_data_completed %>%
  group_by(sleep_duration) %>%
  summarise(counts=n(),
            percentage=n()/nrow(daily_data_completed)) %>%
  ggplot(aes(x='', y=percentage, fill=sleep_duration)) +
  geom_bar(width=2, stat='identity', color='white') +
  coord_polar('y', start=0) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(), 
        panel.grid=element_blank(), 
        axis.ticks=element_blank(),
        axis.text.x=element_blank(),
        plot.title=element_text(hjust = 0.5, size=14, face="bold")) +
  scale_fill_manual(values=c('#3cba54', '#4885ed', '#db3236', '#f4c20d')) +
  geom_text(aes(label=paste0(round(percentage*100), '%')), position=position_stack(vjust=0.5)) +
  labs(title=" Figure 4. Breakdown of Fitbit users' hours of sleep")

daily_data_completed %>%
  group_by(day_of_the_week) %>%
  summarise(avg_sedentary_mins=mean(sedentary_mins)) %>%
  ggplot(aes(x=reorder(day_of_the_week, avg_sedentary_mins), y=avg_sedentary_mins, fill=avg_sedentary_mins)) +
  geom_bar(width=0.2, stat='identity') + geom_col(width=0.4) + coord_flip() + 
  labs(title="Figure 5. Average Sedentary Minutes of Fitbit Users",
       x=element_blank(), y="Average Sedentary Minutes")


# 5. SHARE


# 6. ACT

