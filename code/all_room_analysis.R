#### All Room: 2,3,8,11 analysis ####

source("./rfid_functions.R")

# Generate Transition tables from Room 2
library(xts)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tsibble)

room_2 <- read.csv("../data/DK20-03-RFID-R2-febmay-080423.csv")

bird_ids_room_2 <- unique(room_2$tagname)
bird_ids_room_2 <- na.trim(sort(bird_ids_room_2))

room_2["DateTime"] <- as.POSIXct(room_2$access, origin="1970-01-01", tz="GMT")


print("what makes up subzone col")
unique(room_2$subzone)

room_2$subzone[room_2$subzone == "Bottom"] <- "bottom"
room_2$subzone[room_2$subzone == "Middle"] <- "middle"
room_2$subzone[room_2$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(room_2$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(room_2$DateTime))
sum(is.na(room_2$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
room_2$accessdate <- ymd_hms(room_2$DateTime)

room_2_summary <- room_2 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
 mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
 mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) )) |>
 mutate(intervals_s = map(tsibble, ~ as.numeric(difftime(.x$datetime[1:(length(.x$datetime)-1)], .x$datetime[2:length(.x$datetime)],units='secs') ))) |>
 mutate(summary = map(intervals_s, ~summary(.x))) |>
 mutate(minimum = map(summary, ~abs(.x[6]))) |>
 mutate(median = map(summary, ~.x[3])) |>
 mutate(mean = map(summary, ~.x[4])) |>
 mutate(first_rec =map(tsibble, ~head(.x$datetime, n=1))) |>
 mutate(last_rec = map(tsibble, ~tail(.x$datetime, n=1))) |> 
 unnest(c(first_rec, last_rec, minimum, median,mean))

# Time in minutes and min in sec of average duration between transitions
(mean(room_2_summary$median)/60)
(mean(room_2_summary$mean)/60)
(min(room_2_summary$minimum))

# when is a nice time to start the study?

# All Rooms 2021-03-09 T20:00:00/2021-05-06 T23:00:00
# No Room 3 2021-02-18 T23:30:00/2021-05-06 T23:00:00

room_2_struct <- room_2 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
 mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
 mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

room_2_all_analysis <- room_2_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-18 T23:30:00", "2021-05-06 T23:00:00")))

(room_2_boundries <- data.frame(room_2_summary$tagname, room_2_summary$first_rec, room_2_summary$last_rec))

# All Room Room 2 Time Budget Analysis

room_2_all_analysis <- room_2_all_analysis |>
 filter(!is.na(slicedTsibble)) |> 
 filter(!(tagname %in% c("6910","6966","6914","9005")))
  
# interpolate the rest of the intervals
room_2_regular <- room_2_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 

room_2_dupes <- room_2_regular |>
  select(tagname, sampled) |> 
  mutate(duplicates = map(sampled, ~duplicates(.x)))

room_2_interval <- room_2_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x))) 

# TODO need to set start and end timepoints for this dataset.
room_2_all_room_time_budget <- room_2_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

room_2_all_room_time_budget |>
 select(c(tagname, Interval.1., Interval.2., X1, X2, X3)) |> 
 write.csv(row.names=F, '../output/all_rooms/room_2_all_room_time_budget.csv')

room_2_interval |> 
 unnest(interval) |>
 select(c(tagname, t1,t2,to_zone)) |> 
 write.csv(row.names=F,'../output/all_rooms/room_2_all_room_interval_tab.csv')


for(i in 1:length(room_2_struct$tagname)){

    current_tag <- room_2_struct$tagname[i]

    current_tsibble <- room_2_struct |>
        slice(i) |> 
        pull(tsibble) |> 
        pluck(1)

    current_tsibble$tagname <- rep(current_tag, length(current_tsibble$datetime))

    write.csv(current_tsibble, paste0("../intermediate/all_rooms/room_2_tsibble_",current_tag,".csv"),row.names=F)
}


# All Room Room 2 Daily Time Budget Analysis

# Make day and night "raw data" tables

room_2_all_room_day <- room_2_interval |>
  mutate(day = map(sampled, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(sampled, ~ getNightRecords(.x,"05:00","22:00"))) 

# Turn day and night tables into daily interval tables

room_2_all_room_day <- room_2_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))


# Run getTimeBudgetProp for each daily interval tables

 room_2_all_room_time_budget <- room_2_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetPropDayNight(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetPropDayNight(.x))))


for(i in 1:length(room_2_all_room_time_budget$tagname)){

    current_tag <- room_2_all_room_time_budget$tagname[i]

    current_day_tb<- room_2_all_room_time_budget |>
        slice(i) |> 
        pull(daily_tb) |> 
        pluck(1)

    current_day_tb_df <- do.call(rbind, current_day_tb)
    columns = c("interval1","interval2","Bottom","Middle","Top")

    colnames(current_day_tb_df) <- columns

    current_day_tb_df$tagname <- rep(current_tag, length(current_day_tb_df$interval1))

    write.csv(current_day_tb_df, paste0("../intermediate/all_rooms/room_2_day_time_budget_",current_tag,".csv"),row.names=F)

    current_night_tb <- room_2_all_room_time_budget |>
        slice(i) |> 
        pull(night_tb) |> 
        pluck(1)

    current_night_tb_df <- do.call(rbind, current_night_tb)
    columns = c("interval1","interval2","Bottom","Middle","Top")

    colnames(current_night_tb_df) <- columns

    current_night_tb_df$tagname <- rep(current_tag, length(current_night_tb_df$interval1))

    write.csv(current_night_tb_df, paste0("../intermediate/all_rooms/room_2_night_time_budget_",current_tag,".csv"),row.names=F)
}

# Read in generated time budgets and plot 

library(readr)
library(ggplot2)

day_tbs <- Sys.glob("../intermediate/all_rooms/room_2_day_time_budget_*")

night_tbs <- Sys.glob("../intermediate/all_rooms/room_2_night_time_budget_*")

day_tbs_df <- read_csv(day_tbs)

nest_day_tbs <- day_tbs_df |>
  nest(data = -tagname)

night_tbs_df <- read_csv(night_tbs)

nest_night_tbs <- night_tbs_df |>
   nest(data = - tagname)

# Daytime Plots 05:00-22:00

for(i in 1:length(nest_day_tbs$tagname)){

sb_data <- cbind(nest_day_tbs$data[[i]][1:2], stack(nest_day_tbs$data[[i]][3:5]))

sb_data$ind <- factor(sb_data$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="1 days")

# day_3_plot <- ggplot(data= sb_data, aes(x = as.Date(interval1), y= values, group=ind, color=ind)) +
#   geom_point() + 
#   theme_bw() + 
#   scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   xlab("Day of Study") + 
#   ylab("Prop. of Time Spent in Zone") +
#   ggtitle(paste0("Daily Time Budget for Each Day for Bird ID: ", nest_day_tbs[i,1])) + 
#   scale_colour_discrete( name ="Zone") +
#   scale_y_continuous(limits=c(0, 1.001))

#ggsave(paste0("../figures/all_day/day_daily_time_budget_point_for_", nest_day_tbs[i,1],".png"), day_3_plot)

# day_3_plot_line <- ggplot(data= sb_data, aes(x = as.Date(interval1), y= values, group=ind, color=ind)) +
#   geom_line() + 
#   theme_bw() + 
#   scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   xlab("Day of Study") + 
#   ylab("Prop. of Time Spent in Zone") +
#   ggtitle(paste0("Daily Time Budget for Each Day for Bird ID: ", nest_day_tbs[i,1])) + 
#   scale_colour_discrete( name ="Zone") +
#   scale_y_continuous(limits=c(0, 1.001))

#ggsave(paste0("../figures/all_day/day_daily_time_budget_line_for_", nest_day_tbs[i,1],".png"), day_3_plot_line)

day_3_sb_plot <- ggplot(data = sb_data, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle(paste0("Daily Time Budget for Each Day for Bird ID: ", nest_day_tbs[i,1])) + 
labs(fill = "Zone") + 
scale_y_continuous(limits=c(0, 1.001))

ggsave(paste0("../figures/all_day/room2/day_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
}

# Night Plots 22:01-4:59

for(i in 1:length(nest_night_tbs$tagname)){

sb_data <- cbind(nest_night_tbs$data[[i]][1:2], stack(nest_night_tbs$data[[i]][3:5]))

sb_data$ind <- factor(sb_data$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="1 days")

# day_3_plot <- ggplot(data= sb_data, aes(x = as.Date(interval1), y= values, group=ind, color=ind)) +
#   geom_point() + 
#   theme_bw() + 
#   scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   xlab("Day of Study") + 
#   ylab("Prop. of Time Spent in Zone") +
#   ggtitle(paste0("Daily Time Budget for Each Night for Bird ID: ", nest_day_tbs[i,1])) + 
#   scale_colour_discrete( name ="Zone") +
#   scale_y_continuous(limits=c(0, 1.001))

#ggsave(paste0("../figures/all_day/night_daily_time_budget_point_for_", nest_day_tbs[i,1],".png"), day_3_plot)

# day_3_plot_line <- ggplot(data= sb_data, aes(x = as.Date(interval1), y= values, group=ind, color=ind)) +
#   geom_line() + 
#   theme_bw() + 
#   scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   xlab("Day of Study") + 
#   ylab("Prop. of Time Spent in Zone") +
#   ggtitle(paste0("Daily Time Budget for Each Night for Bird ID: ", nest_day_tbs[i,1])) + 
#   scale_colour_discrete( name ="Zone") +
#   scale_y_continuous(limits=c(0, 1.001))

#ggsave(paste0("../figures/all_day/night_daily_time_budget_line_for_", nest_day_tbs[i,1],".png"), day_3_plot_line)

day_3_sb_plot <- ggplot(data = sb_data, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle(paste0("Daily Time Budget for Each Night for Bird ID: ", nest_day_tbs[i,1])) + 
labs(fill = "Zone") + 
scale_y_continuous(limits=c(0, 1.001))

ggsave(paste0("../figures/all_day/room2/night_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
}

# Averaged Birds in Room 2 Daily Time Budget

day_tbs_df 
day_flat <- cbind(day_tbs_df[c(1:2,6)], stack(day_tbs_df[3:5]))

day_flat$ind <- factor(day_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="1 days")

# TODO  Still having a problem
room_2_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Day for Room 2") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, 11.001))

ggsave(paste0("../figures/all_day/room2/day_daily_time_budget_stack_bar_for_room_2",".png"), room_2_sb_plot)


# room_2_bp <- ggplot(day_flat, aes(x = factor(as.Date(interval1)), y=values, fill=ind)) + 
#   geom_boxplot()

# ggsave(paste0("../figures/all_day/day_daily_time_budget_boxplot_for_room_2",".png"), room_2_bp)

# Averaged Birds in Room 2 Nightly Time Budget

night_flat <- cbind(night_tbs_df[c(1:2,6)], stack(night_tbs_df[3:5]))

night_flat$ind <- factor(night_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="1 days")

# TODO  Still having a problem
room_2_sb_night_plot <- ggplot(data = night_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Night for Room 2") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, 11.001))

ggsave(paste0("../figures/all_day/room2/night_daily_time_budget_stack_bar_for_room_2",".png"), room_2_sb_night_plot)

### END OF ROOM 2 ANALYSIS ###

######## Room 3 Individual Analysis ########

room_3 <- read.csv("../data/DK20-03-RFID-R3-febmay-080423.csv") %>% na.exclude()

bird_ids_room_3 <- unique(room_3$tagname)
bird_ids_room_3 <- na.trim(sort(bird_ids_room_3))

room_3["DateTime"] <- as.POSIXct(room_3$access, origin="1970-01-01", tz="GMT")


print("what makes up subzone col")
unique(room_3$subzone)

room_3$subzone[room_3$subzone == "Bottom"] <- "bottom"
room_3$subzone[room_3$subzone == "Middle"] <- "middle"
room_3$subzone[room_3$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(room_3$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(room_3$DateTime))
sum(is.na(room_3$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
room_3$accessdate <- ymd_hms(room_3$DateTime)

room_3_summary <- room_3 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
 mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
 mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) )) |>
 mutate(intervals_s = map(tsibble, ~ as.numeric(difftime(.x$datetime[1:(length(.x$datetime)-1)], .x$datetime[2:length(.x$datetime)],units='secs') ))) |>
 mutate(summary = map(intervals_s, ~summary(.x))) |>
 mutate(minimum = map(summary, ~abs(.x[6]))) |>
 mutate(median = map(summary, ~.x[3])) |>
 mutate(mean = map(summary, ~.x[4])) |>
 mutate(first_rec =map(tsibble, ~head(.x$datetime, n=1))) |>
 mutate(last_rec = map(tsibble, ~tail(.x$datetime, n=1))) |> 
 unnest(c(first_rec, last_rec, minimum, median,mean))

# Time in minutes and min in sec of average duration between transitions
(mean(room_3_summary$median)/60)
(mean(room_3_summary$mean)/60)
(min(room_3_summary$minimum))

# when is a nice time to start the study?

# All Rooms 2021-03-09 T20:00:00/2021-05-06 T23:00:00
# No Room 3 2021-02-18 T23:30:00/2021-05-06 T23:00:00

room_3_struct <- room_3 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
 mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
 mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

room_3_all_analysis <- room_3_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-18 T23:30:00", "2021-05-06 T23:00:00")))

(room_3_boundries <- data.frame(room_3_summary$tagname, room_3_summary$first_rec, room_3_summary$last_rec))

# All Room room_3 Time Budget Analysis

room_3_all_analysis <- room_3_all_analysis |>
 filter(!is.na(slicedTsibble)) |> 
 filter(!(tagname %in% c("6968","6879","6948","6915","6953","6987","9008","6912","9029")))
  
# interpolate the rest of the intervals
room_3_regular <- room_3_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 

room_3_dupes <- room_3_regular |>
  select(tagname, sampled) |> 
  mutate(duplicates = map(sampled, ~duplicates(.x)))

room_3_interval <- room_3_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x))) 

# TODO need to set start and end timepoints for this dataset.
room_3_all_room_time_budget <- room_3_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

room_3_all_room_time_budget |>
 select(c(tagname, Interval.1., Interval.2., X1, X2, X3)) |> 
 write.csv(row.names=F, '../output/all_rooms/room_3_all_room_time_budget.csv')

room_3_interval |> 
 unnest(interval) |>
 select(c(tagname, t1,t2,to_zone)) |> 
 write.csv(row.names=F,'../output/all_rooms/room_3_all_room_interval_tab.csv')


for(i in 1:length(room_3_struct$tagname)){

    current_tag <- room_3_struct$tagname[i]

    current_tsibble <- room_3_struct |>
        slice(i) |> 
        pull(tsibble) |> 
        pluck(1)

    current_tsibble$tagname <- rep(current_tag, length(current_tsibble$datetime))

    write.csv(current_tsibble, paste0("../intermediate/all_rooms/room_3_tsibble_",current_tag,".csv"),row.names=F)
}



# All Room room_3 Daily Time Budget Analysis

# Make day and night "raw data" tables

room_3_all_room_day <- room_3_interval |>
  mutate(day = map(sampled, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(sampled, ~ getNightRecords(.x,"05:00","22:00"))) 

# Turn day and night tables into daily interval tables

room_3_all_room_day <- room_3_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))


# Run getTimeBudgetProp for each daily interval tables

 room_3_all_room_time_budget <- room_3_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetPropDayNight(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetPropDayNight(.x))))


for(i in 1:length(room_3_all_room_time_budget$tagname)){

    current_tag <- room_3_all_room_time_budget$tagname[i]

    current_day_tb<- room_3_all_room_time_budget |>
        slice(i) |> 
        pull(daily_tb) |> 
        pluck(1)

    current_day_tb_df <- do.call(rbind, current_day_tb)
    columns = c("interval1","interval2","Bottom","Middle","Top")

    colnames(current_day_tb_df) <- columns

    current_day_tb_df$tagname <- rep(current_tag, length(current_day_tb_df$interval1))

    write.csv(current_day_tb_df, paste0("../intermediate/all_rooms/room_3_day_time_budget_",current_tag,".csv"),row.names=F)

    current_night_tb <- room_3_all_room_time_budget |>
        slice(i) |> 
        pull(night_tb) |> 
        pluck(1)

    current_night_tb_df <- do.call(rbind, current_night_tb)
    columns = c("interval1","interval2","Bottom","Middle","Top")

    colnames(current_night_tb_df) <- columns

    current_night_tb_df$tagname <- rep(current_tag, length(current_night_tb_df$interval1))

    write.csv(current_night_tb_df, paste0("../intermediate/all_rooms/room_3_night_time_budget_",current_tag,".csv"),row.names=F)
}

# Read in generated time budgets and plot 

library(readr)
library(ggplot2)

day_tbs <- Sys.glob("../intermediate/all_rooms/room_3_day_time_budget_*")

night_tbs <- Sys.glob("../intermediate/all_rooms/room_3_night_time_budget_*")

day_tbs_df <- read_csv(day_tbs)

nest_day_tbs <- day_tbs_df |>
  nest(data = -tagname)

night_tbs_df <- read_csv(night_tbs)

nest_night_tbs <- night_tbs_df |>
   nest(data = - tagname)

# Daytime Plots 05:00-22:00

for(i in 1:length(nest_day_tbs$tagname)){

sb_data <- cbind(nest_day_tbs$data[[i]][1:2], stack(nest_day_tbs$data[[i]][3:5]))

sb_data$ind <- factor(sb_data$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="1 days")


day_3_sb_plot <- ggplot(data = sb_data, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle(paste0("Daily Time Budget for Each Day for Bird ID: ", nest_day_tbs[i,1])) + 
labs(fill = "Zone") + 
scale_y_continuous(limits=c(0, 1.001))

ggsave(paste0("../figures/all_day/day_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
}

# Night Plots 22:01-4:59

for(i in 1:length(nest_night_tbs$tagname)){

sb_data <- cbind(nest_night_tbs$data[[i]][1:2], stack(nest_night_tbs$data[[i]][3:5]))

sb_data$ind <- factor(sb_data$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="1 days")

day_3_sb_plot <- ggplot(data = sb_data, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle(paste0("Daily Time Budget for Each Night for Bird ID: ", nest_day_tbs[i,1])) + 
labs(fill = "Zone") + 
scale_y_continuous(limits=c(0, 1.001))

ggsave(paste0("../figures/all_day/room3/night_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
}

# Averaged Birds in room_3 Daily Time Budget

day_tbs_df 
day_flat <- cbind(day_tbs_df[c(1:2,6)], stack(day_tbs_df[3:5]))

day_flat$ind <- factor(day_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="1 days")

# TODO  Still having a problem
room_3_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Day for room_3") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, 11.001))

ggsave(paste0("../figures/all_day/room3/day_daily_time_budget_stack_bar_for_room_3",".png"), room_3_sb_plot)


# room_3_bp <- ggplot(day_flat, aes(x = factor(as.Date(interval1)), y=values, fill=ind)) + 
#   geom_boxplot()

# ggsave(paste0("../figures/all_day/day_daily_time_budget_boxplot_for_room_3",".png"), room_3_bp)

# Averaged Birds in room_3 Nightly Time Budget

night_flat <- cbind(night_tbs_df[c(1:2,6)], stack(night_tbs_df[3:5]))

night_flat$ind <- factor(night_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="1 days")

# TODO  Still having a problem
room_3_sb_night_plot <- ggplot(data = night_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Night for Room 3") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, 11.001))

ggsave(paste0("../figures/all_day/room3/night_daily_time_budget_stack_bar_for_room_3",".png"), room_3_sb_night_plot)

### END OF ROOM 3 ANALYSIS ###

######## Room 8 Individual Analysis ########
# TODO Update this for room 8

room_3 <- read.csv("../data/DK20-03-RFID-R3-febmay-080423.csv") %>% na.exclude()

bird_ids_room_3 <- unique(room_3$tagname)
bird_ids_room_3 <- na.trim(sort(bird_ids_room_3))

room_3["DateTime"] <- as.POSIXct(room_3$access, origin="1970-01-01", tz="GMT")


print("what makes up subzone col")
unique(room_3$subzone)

room_3$subzone[room_3$subzone == "Bottom"] <- "bottom"
room_3$subzone[room_3$subzone == "Middle"] <- "middle"
room_3$subzone[room_3$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(room_3$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(room_3$DateTime))
sum(is.na(room_3$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
room_3$accessdate <- ymd_hms(room_3$DateTime)

room_3_summary <- room_3 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
 mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
 mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) )) |>
 mutate(intervals_s = map(tsibble, ~ as.numeric(difftime(.x$datetime[1:(length(.x$datetime)-1)], .x$datetime[2:length(.x$datetime)],units='secs') ))) |>
 mutate(summary = map(intervals_s, ~summary(.x))) |>
 mutate(minimum = map(summary, ~abs(.x[6]))) |>
 mutate(median = map(summary, ~.x[3])) |>
 mutate(mean = map(summary, ~.x[4])) |>
 mutate(first_rec =map(tsibble, ~head(.x$datetime, n=1))) |>
 mutate(last_rec = map(tsibble, ~tail(.x$datetime, n=1))) |> 
 unnest(c(first_rec, last_rec, minimum, median,mean))

# Time in minutes and min in sec of average duration between transitions
(mean(room_3_summary$median)/60)
(mean(room_3_summary$mean)/60)
(min(room_3_summary$minimum))

# when is a nice time to start the study?

# All Rooms 2021-03-09 T20:00:00/2021-05-06 T23:00:00
# No Room 3 2021-02-18 T23:30:00/2021-05-06 T23:00:00

room_3_struct <- room_3 |> nest(data = - tagname) |> 
 na.exclude() |>
 mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
 mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
 mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

room_3_all_analysis <- room_3_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-02-18 T23:30:00", "2021-05-06 T23:00:00")))

(room_3_boundries <- data.frame(room_3_summary$tagname, room_3_summary$first_rec, room_3_summary$last_rec))

# All Room room_3 Time Budget Analysis

# TODO update the id's from room 8 
room_3_all_analysis <- room_3_all_analysis |>
 filter(!is.na(slicedTsibble)) |> 
 filter(!(tagname %in% c("")))
  
# interpolate the rest of the intervals
room_3_regular <- room_3_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(perSec = map(near_5, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 

room_3_dupes <- room_3_regular |>
  select(tagname, sampled) |> 
  mutate(duplicates = map(sampled, ~duplicates(.x)))

room_3_interval <- room_3_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x))) 

# TODO need to set start and end timepoints for this dataset.
room_3_all_room_time_budget <- room_3_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

room_3_all_room_time_budget |>
 select(c(tagname, Interval.1., Interval.2., X1, X2, X3)) |> 
 write.csv(row.names=F, '../output/all_rooms/room_3_all_room_time_budget.csv')

room_3_interval |> 
 unnest(interval) |>
 select(c(tagname, t1,t2,to_zone)) |> 
 write.csv(row.names=F,'../output/all_rooms/room_3_all_room_interval_tab.csv')


for(i in 1:length(room_3_struct$tagname)){

    current_tag <- room_3_struct$tagname[i]

    current_tsibble <- room_3_struct |>
        slice(i) |> 
        pull(tsibble) |> 
        pluck(1)

    current_tsibble$tagname <- rep(current_tag, length(current_tsibble$datetime))

    write.csv(current_tsibble, paste0("../intermediate/all_rooms/room_3_tsibble_",current_tag,".csv"),row.names=F)
}



# All Room room_3 Daily Time Budget Analysis

# Make day and night "raw data" tables

room_3_all_room_day <- room_3_interval |>
  mutate(day = map(sampled, ~ getDayRecords(.x,"05:00","22:00"))) |>
  mutate(night = map(sampled, ~ getNightRecords(.x,"05:00","22:00"))) 

# Turn day and night tables into daily interval tables

room_3_all_room_day <- room_3_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))


# Run getTimeBudgetProp for each daily interval tables

 room_3_all_room_time_budget <- room_3_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetPropDayNight(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetPropDayNight(.x))))


for(i in 1:length(room_3_all_room_time_budget$tagname)){

    current_tag <- room_3_all_room_time_budget$tagname[i]

    current_day_tb<- room_3_all_room_time_budget |>
        slice(i) |> 
        pull(daily_tb) |> 
        pluck(1)

    current_day_tb_df <- do.call(rbind, current_day_tb)
    columns = c("interval1","interval2","Bottom","Middle","Top")

    colnames(current_day_tb_df) <- columns

    current_day_tb_df$tagname <- rep(current_tag, length(current_day_tb_df$interval1))

    write.csv(current_day_tb_df, paste0("../intermediate/all_rooms/room_3_day_time_budget_",current_tag,".csv"),row.names=F)

    current_night_tb <- room_3_all_room_time_budget |>
        slice(i) |> 
        pull(night_tb) |> 
        pluck(1)

    current_night_tb_df <- do.call(rbind, current_night_tb)
    columns = c("interval1","interval2","Bottom","Middle","Top")

    colnames(current_night_tb_df) <- columns

    current_night_tb_df$tagname <- rep(current_tag, length(current_night_tb_df$interval1))

    write.csv(current_night_tb_df, paste0("../intermediate/all_rooms/room_3_night_time_budget_",current_tag,".csv"),row.names=F)
}

# Read in generated time budgets and plot 

library(readr)
library(ggplot2)

day_tbs <- Sys.glob("../intermediate/all_rooms/room_3_day_time_budget_*")

night_tbs <- Sys.glob("../intermediate/all_rooms/room_3_night_time_budget_*")

day_tbs_df <- read_csv(day_tbs)

nest_day_tbs <- day_tbs_df |>
  nest(data = -tagname)

night_tbs_df <- read_csv(night_tbs)

nest_night_tbs <- night_tbs_df |>
   nest(data = - tagname)

# Daytime Plots 05:00-22:00

for(i in 1:length(nest_day_tbs$tagname)){

sb_data <- cbind(nest_day_tbs$data[[i]][1:2], stack(nest_day_tbs$data[[i]][3:5]))

sb_data$ind <- factor(sb_data$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="1 days")


day_3_sb_plot <- ggplot(data = sb_data, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle(paste0("Daily Time Budget for Each Day for Bird ID: ", nest_day_tbs[i,1])) + 
labs(fill = "Zone") + 
scale_y_continuous(limits=c(0, 1.001))

ggsave(paste0("../figures/all_day/day_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
}

# Night Plots 22:01-4:59

for(i in 1:length(nest_night_tbs$tagname)){

sb_data <- cbind(nest_night_tbs$data[[i]][1:2], stack(nest_night_tbs$data[[i]][3:5]))

sb_data$ind <- factor(sb_data$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="1 days")

day_3_sb_plot <- ggplot(data = sb_data, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle(paste0("Daily Time Budget for Each Night for Bird ID: ", nest_day_tbs[i,1])) + 
labs(fill = "Zone") + 
scale_y_continuous(limits=c(0, 1.001))

ggsave(paste0("../figures/all_day/room3/night_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
}

# Averaged Birds in room_3 Daily Time Budget

day_tbs_df 
day_flat <- cbind(day_tbs_df[c(1:2,6)], stack(day_tbs_df[3:5]))

day_flat$ind <- factor(day_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="1 days")

# TODO  Still having a problem
room_3_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Day for room_3") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, 11.001))

ggsave(paste0("../figures/all_day/room3/day_daily_time_budget_stack_bar_for_room_3",".png"), room_3_sb_plot)


# room_3_bp <- ggplot(day_flat, aes(x = factor(as.Date(interval1)), y=values, fill=ind)) + 
#   geom_boxplot()

# ggsave(paste0("../figures/all_day/day_daily_time_budget_boxplot_for_room_3",".png"), room_3_bp)

# Averaged Birds in room_3 Nightly Time Budget

night_flat <- cbind(night_tbs_df[c(1:2,6)], stack(night_tbs_df[3:5]))

night_flat$ind <- factor(night_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="1 days")

# TODO  Still having a problem
room_3_sb_night_plot <- ggplot(data = night_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Night for Room 3") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, 11.001))

ggsave(paste0("../figures/all_day/room3/night_daily_time_budget_stack_bar_for_room_3",".png"), room_3_sb_night_plot)

### END OF ROOM 8 ANALYSIS ###