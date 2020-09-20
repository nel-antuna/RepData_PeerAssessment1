

# Loading and preprocessing the data --------------------------------------

library(tidyverse)
options(max.print = 50, scipen = 999)

unzip("./activity.zip")
activity <- read.csv("./activity.csv")
activity$date <- parse_date(activity$date, "%Y-%m-%d")

# What is mean total number of steps taken per day ------------------------

steps_by_day <- activity %>%
  group_by(date) %>%
  summarize(
    total_steps = sum(steps, na.rm = TRUE)
  )

ggplot(steps_by_day, aes(total_steps))+
  geom_histogram()+
  theme_minimal()+
  labs(x="Steps Taken", y="Number Days", title="Number of Days with same Step Count")
  
mean_steps_by_date <- sum(activity$steps,na.rm = TRUE)/length(unique(activity$date))

median_steps_by_date <- median(steps_by_day$total_steps,na.rm=TRUE)


# What is the average daily activity pattern? -----------------------------

steps_by_interval <- activity %>%
  group_by(interval)%>%
  summarize(
    avg_steps = mean(steps,na.rm = TRUE)
  )

ggplot(steps_by_interval,aes(interval,avg_steps))+
  geom_line()+
  theme_minimal()+
  labs(x="Time Interval",y="Average Steps","Average Steps per Time Interval")

max_interval <- arrange(steps_by_interval,desc(avg_steps))[1,1]


# Imputing missing values --------------------------------------------------

total_nas <- sum(is.na(activity$steps))

activity_revised <- activity
  
for(i in 1:length(activity_revised$steps)){
  if(is.na(activity_revised$steps[i]) == TRUE) {
    activity_revised$steps[i] = filter(steps_by_interval, interval == activity_revised$interval[i])[[1,2]]
  } 
} 

steps_by_day_revised <- activity_revised %>%
  group_by(date) %>%
  summarize(
    total_steps = sum(steps, na.rm = TRUE)
  )

ggplot(steps_by_day_revised, aes(total_steps))+
  geom_histogram()+
  theme_minimal()+
  labs(x="Steps Taken", y="Number Days", title="Number of Days with same Step Count")

mean_steps_by_date_revised <- sum(activity_revised$steps,na.rm = TRUE)/length(unique(activity_revised$date))

median_steps_by_date_revised <- median(steps_by_day_revised$total_steps,na.rm=TRUE)


# Are there differences in activity patterns between weekdays and  --------

day_week <- seq_along(activity_revised$date)

for(i in 1:length(activity_revised$date)){
  if(weekdays(activity_revised$date[i]) %in% c("Saturday","Sunday")) {
    day_week[i] = "Weekend"
  } else {
    day_week[i] = "Weekday"
  }
}

day_week <- factor(day_week,c("Weekday","Weekend"))

activity_revised$day_week <- day_week

head(activity_revised)

steps_by_intervay_and_weekday <- activity_revised %>%
  group_by(interval, day_week) %>%
  summarize(
    avg_steps = mean(steps)
  )

ggplot(steps_by_intervay_and_weekday, aes(interval,avg_steps))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~day_week,nrow=2)+
  labs(x="Interval",y="Number of Steps")
