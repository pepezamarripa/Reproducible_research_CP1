# Pepe Zamarripa
# Reproducible research - Peer assessment 1
# June 1st, 2023

library("data.table")
library(ggplot2)

## 1: Code for reading in the dataset and/or processing the data

#Load data
getFile <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(getFile, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

#Read the .csv file 
activityDT <- data.table::fread(input = "data/activity.csv")

## 2: Histogram of the total number of steps taken each day

total_steps <- activityDT[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 

head(total_steps, 10)

ggplot(total_steps, aes(x = steps)) + geom_histogram(fill = "Tomato3", binwidth = 1000) + labs(title = "Daily steps", x = "Steps", y = "Frequency")


## 3: Mean and median number of steps taken each day
total_steps[, .(mean_steps = mean(steps, na.rm = TRUE), median_steps = median(steps, na.rm = TRUE))]

## 4: Time series plot of the average number of steps taken
interval_DT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
ggplot(interval_DT, aes(x = interval , y = steps)) + geom_line(color="Tomato3", size=1) + labs(title = "Avg. daily steps", x = "Interval", y = "Avg. steps per day")


## 5: The 5-minute interval that, on average, contains the maximum number of steps
interval_DT[steps == max(steps), .(max_interval = interval)]

## 6: Code to describe and show a strategy for imputing missing data

# Get the total number of missing elements
activityDT[is.na(steps), .N ]

# Filling in missing values with median of dataset. 
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
data.table::fwrite(x = activityDT, file = "data/tidyData.csv", quote = FALSE)

# total number of steps taken per day
total_steps <- activityDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

## 7: Histogram of the total number of steps taken each day after missing values are imputed

# mean and median total number of steps taken per day
total_steps[, .(mean_steps = mean(steps), median_steps = median(steps))]

ggplot(total_steps, aes(x = steps)) + geom_histogram(fill = "Tomato3", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")

## 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
interval_DT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(interval_DT , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
