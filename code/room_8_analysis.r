######## Room 8 Individual Analysis ########

source("./rfid_functions.R")

# Generate Transition tables from Room 8
library(xts)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tsibble)


room_8 <- read.csv("../data/set_2/DK20-03-RFID-r8-febmay-080423.csv") %>% na.exclude()

bird_ids_room_8 <- unique(room_8$tagname)
bird_ids_room_8 <- na.trim(sort(bird_ids_room_8))

room_8["DateTime"] <- as.POSIXct(room_8$access, origin="1970-01-01", tz="GMT")


print("what makes up subzone col")
unique(room_8$subzone)

room_8$subzone[room_8$subzone == "Bottom"] <- "bottom"
room_8$subzone[room_8$subzone == "Middle"] <- "middle"
room_8$subzone[room_8$subzone == "Top"] <- "top"


print("what makes up subzone col")
unique(room_8$subzone)

print("how many NAs in DateTime and Subzone")
sum(is.na(room_8$DateTime))
sum(is.na(room_8$subzone))

# This is a hack to work with the downloaded data from excel and onedrive
room_8$accessdate <- ymd_hms(room_8$DateTime)

room_8_summary <- room_8 |> nest(data = - tagname) |> 
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
print("median, mean, minimum ; duration between transitions (sec)")
(mean(room_8_summary$median)/60)
(mean(room_8_summary$mean)/60)
(min(room_8_summary$minimum))

# when is a nice time to start the study?

# All Rooms 2021-03-09 T20:00:00/2021-05-06 T23:00:00
# No Room 3 2021-02-18 T23:30:00/2021-05-06 T23:00:00

room_8_struct <- room_8 |> nest(data = - tagname) |> 
 na.exclude() |>
#  mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
 mutate(cleaned = map(data, ~distinct(.x, accessdate, .keep_all=TRUE))) |>
 mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

room_8_all_analysis <- room_8_struct |>
 mutate(slicedTsibble = map(tsibble, ~ sliceTsibble(.x, "2021-03-10 T04:00:00", "2021-05-06 T22:00:00")))

(room_8_boundries <- data.frame(room_8_summary$tagname, room_8_summary$first_rec, room_8_summary$last_rec))

print("to be removed: ")

(room_8_out <- rbind(room_8_boundries[!room_8_boundries$room_8_summary.first_rec < "2021-03-10 T04:00:00",],room_8_boundries[!room_8_boundries$room_8_summary.last_rec > "2021-05-06 T22:00:00",]))


# All Room room_8 Time Budget Analysis

# TODO update the id's from room 8 
room_8_all_analysis <- room_8_all_analysis |>
 filter(!is.na(slicedTsibble)) |> 
 filter(!(tagname %in% room_8_out$room_8_summary.tagname))
  
# interpolate the rest of the intervals
room_8_regular <- room_8_all_analysis |>
 select(c(tagname, slicedTsibble)) |>
 mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
 mutate(near_5_df= map(near_5, ~tibble(.x))) |>
 nest_mutate(near_5_df, datetime=round_date(datetime,"5 seconds"),value=value) |>
 mutate(near_5_tsibble = map(near_5_df, ~tsibble(.x[!are_duplicated(.x),]))) |> 
 mutate(perSec = map(near_5_tsibble, ~ fill_gaps(.x)))|>
 mutate(sampled = map(perSec, ~ na.locf(.x))) 

room_8_dupes <- room_8_regular |>
  select(tagname, sampled) |> 
  mutate(duplicates = map(sampled, ~duplicates(.x)))

room_8_interval <- room_8_regular |>
  mutate(interval = map(sampled, ~timeToIntervals(.x))) 

# TODO need to set start and end timepoints for this dataset.
room_8_all_room_time_budget <- room_8_interval |>
  mutate(tb = map(interval, ~ getTimeBudgetProp(.x))) |>
  unnest(tb) 

room_8_all_room_time_budget |>
 select(c(tagname, Interval.1., Interval.2., X1, X2, X3)) |> 
 write.csv(row.names=F, '../output/all_rooms/room_8_all_room_time_budget.csv')

room_8_interval |> 
 unnest(interval) |>
 select(c(tagname, t1,t2,to_zone)) |> 
 write.csv(row.names=F,'../output/all_rooms/room_8_all_room_interval_tab.csv')


for(i in 1:length(room_8_struct$tagname)){

    current_tag <- room_8_struct$tagname[i]

    current_tsibble <- room_8_struct |>
        slice(i) |> 
        pull(tsibble) |> 
        pluck(1)

    current_tsibble$tagname <- rep(current_tag, length(current_tsibble$datetime))

    write.csv(current_tsibble, paste0("../intermediate/all_rooms/room_8_tsibble_",current_tag,".csv"),row.names=F)
}



# All Room room_8 Daily Time Budget Analysis

# Make day and night "raw data" tables

room_8_all_room_day <- room_8_interval |>
  mutate(day = map(sampled, ~ getDayRecords(.x,"04:00","22:00"))) |>
  mutate(night = map(sampled, ~ getNightRecords(.x,"04:00","22:00"))) 

# Turn day and night tables into daily interval tables

room_8_all_room_day <- room_8_all_room_day |>
  mutate(day_int = map(day, ~ nestedTimeToIntervals(.x))) |>
  mutate(night_int = map(night, ~ nestedTimeToIntervals(.x)))


# Run getTimeBudgetProp for each daily interval tables

 room_8_all_room_time_budget <- room_8_all_room_day |>
  mutate(daily_tb = map(day_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x)))) |>
  mutate(night_tb = map(night_int, ~ map(.x$daily_int, ~ getTimeBudgetProp(.x))))


for(i in 1:length(room_8_all_room_time_budget$tagname)){

    current_tag <- room_8_all_room_time_budget$tagname[i]

    current_day_tb<- room_8_all_room_time_budget |>
        slice(i) |> 
        pull(daily_tb) |> 
        pluck(1)

    current_day_tb_df <- do.call(rbind, current_day_tb)
    columns = c("interval1","interval2","Bottom","Middle","Top")

    colnames(current_day_tb_df) <- columns

    current_day_tb_df$tagname <- rep(current_tag, length(current_day_tb_df$interval1))

    write.csv(current_day_tb_df, paste0("../intermediate/all_rooms/room_8_day_time_budget_",current_tag,".csv"),row.names=F)

    current_night_tb <- room_8_all_room_time_budget |>
        slice(i) |> 
        pull(night_tb) |> 
        pluck(1)

    current_night_tb_df <- do.call(rbind, current_night_tb)
    columns = c("interval1","interval2","Bottom","Middle","Top")

    colnames(current_night_tb_df) <- columns

    current_night_tb_df$tagname <- rep(current_tag, length(current_night_tb_df$interval1))

    write.csv(current_night_tb_df, paste0("../intermediate/all_rooms/room_8_night_time_budget_",current_tag,".csv"),row.names=F)
}

# Read in generated time budgets and plot 

library(readr)
library(ggplot2)

day_tbs <- Sys.glob("../intermediate/all_rooms/room_8_day_time_budget_*")

night_tbs <- Sys.glob("../intermediate/all_rooms/room_8_night_time_budget_*")

day_tbs_df <- read_csv(day_tbs)

nest_day_tbs <- day_tbs_df |>
  nest(data = -tagname)

night_tbs_df <- read_csv(night_tbs)

nest_night_tbs <- night_tbs_df |>
   nest(data = - tagname)

# Daytime Plots 04:00-22:00

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

ggsave(paste0("../figures/all_day/room8/day_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
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

ggsave(paste0("../figures/all_day/room8/night_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
}

# Averaged Birds in room_8 Daily Time Budget


day_flat <- cbind(day_tbs_df[c(1:2,6)], stack(day_tbs_df[3:5]))

day_flat$ind <- factor(day_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="1 days")

y_lim <- length(unique(day_flat$tagname))+.001

# TODO  Check Limits
room_8_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Day for Room 8") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))

ggsave(paste0("../figures/all_day/room8/day_daily_time_budget_stack_bar_for_room_8",".png"), room_8_sb_plot)


# room_3_bp <- ggplot(day_flat, aes(x = factor(as.Date(interval1)), y=values, fill=ind)) + 
#   geom_boxplot()

# ggsave(paste0("../figures/all_day/day_daily_time_budget_boxplot_for_room_3",".png"), room_3_bp)

# Averaged Birds in room_3 Nightly Time Budget

night_flat <- cbind(night_tbs_df[c(1:2,6)], stack(night_tbs_df[3:5]))

night_flat$ind <- factor(night_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="1 days")

# TODO  Check Limits
room_8_sb_night_plot <- ggplot(data = night_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Night for Room 8") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))

ggsave(paste0("../figures/all_day/room8/night_daily_time_budget_stack_bar_for_room_8",".png"), room_8_sb_night_plot)

### Save Overall Interval, Daily Interval and Nightly Interval to disk ###

# Overall

for(i in 1:length( room_8_interval$tagname)){

    current_tag <- room_8_interval$tagname[i]

    current_tsibble <- room_8_interval |>
        slice(i) |> 
        pull(interval) |> 
        pluck(1)

    current_tsibble$tagname <- rep(current_tag, length(current_tsibble$t1))

    write.csv(current_tsibble, paste0("../intermediate/all_rooms/overall_interval/room_8_interval_",current_tag,".csv"),row.names=F)
}

# Daily 

room_8_day_int <- room_8_all_room_day |>
  mutate(daily_int = map(day, ~timeToIntervals(.x)))

for(i in 1:length(room_8_day_int$tagname)){

    current_tag <- room_8_day_int$tagname[i]

    current_day_tsibble <- room_8_day_int |>
        slice(i) |> 
        pull(daily_int) |> 
        pluck(1)
    
    current_day_tsibble$tagname <- rep(current_tag, length(current_day_tsibble$t1))
  
    write.csv(current_day_tsibble, paste0("../intermediate/all_rooms/daily_interval/room_8_interval_",current_tag,".csv"),row.names=F)
}

# Nightly

room_8_night_int <- room_8_all_room_day |>
  mutate(nightly_int = map(night, ~timeToIntervals(.x)))

for(i in 1:length(room_8_night_int$tagname)){

    current_tag <- room_8_night_int$tagname[i]

    current_night_tsibble <- room_8_night_int |>
        slice(i) |> 
        pull(nightly_int) |> 
        pluck(1)
  
    current_night_tsibble$tagname <- rep(current_tag, length(current_night_tsibble$t1))

    write.csv(current_night_tsibble, paste0("../intermediate/all_rooms/nightly_interval/room_8_interval_",current_tag,".csv"),row.names=F)
}

### END OF ROOM 8 ANALYSIS ###