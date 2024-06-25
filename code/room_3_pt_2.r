library(xts)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tsibble)
library(nplyr)
library(purrr)
library(LTS)

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

# Daytime Plots 04:00-22:00

for(i in 1:length(nest_day_tbs$tagname)){

sb_data <- cbind(nest_day_tbs$data[[i]][1:2], stack(nest_day_tbs$data[[i]][3:5]))

sb_data$ind <- factor(sb_data$ind, levels=c("Top","Middle","Bottom"))

# datebreaks <- seq(as.Date(ymd_hms(as.POSIXct.numeric(head(unique(sb_data$interval1),n=1),origin="1970-01-01",tz="UTC"))), as.Date(ymd_hms(as.POSIXct.numeric(tail(unique(sb_data$interval1),n=1),origin="1970-01-01",tz="UTC"))), by="7 days")

# datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))))

# all_datebreak <- seq(as.Date(ymd_hms(head(unique(sb_data$interval1),n=1))), as.Date(ymd_hms(tail(unique(sb_data$interval1),n=1))), by="1 days")


# datebreaks <- seq(as.Date(ymd_hms(as.POSIXct.numeric(as.numeric(head(unique(sb_data$interval1),n=1)),origin="1970-01-01",tz="UTC"))), as.Date(ymd_hms(as.POSIXct.numeric(as.numeric(tail(unique(sb_data$interval1),n=1)),origin="1970-01-01",tz="UTC"))), by="7 days")

# datebreaks <- c(datebreaks, ymd_hms(as.POSIXct.numeric(as.numeric(tail(unique(sb_data$interval1),n=1)),origin="1970-01-01",tz="UTC")))

# all_datebreak <- seq(as.Date(ymd_hms(as.POSIXct.numeric(as.numeric(head(unique(sb_data$interval1),n=1)),origin="1970-01-01",tz="UTC"))), as.Date(ymd_hms(as.POSIXct.numeric(as.numeric(tail(unique(sb_data$interval1),n=1)),origin="1970-01-01",tz="UTC"))), by="1 days")

datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(sb_data$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(sb_data$interval1),n=1),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(sb_data$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(sb_data$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(sb_data$interval1),n=1),origin="1970-01-01")), by="1 days")



day_3_sb_plot <- ggplot(data = sb_data, aes(x = as.Date(as.POSIXct.numeric(as.numeric(interval1),origin="1970-01-01")), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= as.Date(datebreaks), minor_breaks=as.Date(all_datebreak)) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle(paste0("Daily Time Budget for Each Day for Bird ID: ", nest_day_tbs[i,1])) + 
labs(fill = "Zone") + 
scale_y_continuous(limits=c(0, 1.001))

ggsave(paste0("../figures/all_day/room3/day_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
}

# Night Plots 22:01-4:59

for(i in 1:length(nest_night_tbs$tagname)){

sb_data <- cbind(nest_night_tbs$data[[i]][1:2], stack(nest_night_tbs$data[[i]][3:5]))

sb_data$ind <- factor(sb_data$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(sb_data$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(sb_data$interval1),n=1),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(sb_data$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(sb_data$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(sb_data$interval1),n=1),origin="1970-01-01")), by="1 days")

day_3_sb_plot <- ggplot(data = sb_data, aes(x = as.Date(as.POSIXct.numeric(as.numeric(interval1),origin="1970-01-01")), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= as.Date(datebreaks), minor_breaks=as.Date(all_datebreak)) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle(paste0("Daily Time Budget for Each Night for Bird ID: ", nest_day_tbs[i,1])) + 
labs(fill = "Zone") + 
scale_y_continuous(limits=c(0, 1.001))

ggsave(paste0("../figures/all_day/room3/night_daily_time_budget_stack_bar_for_", nest_day_tbs[i,1],".png"), day_3_sb_plot)
}

# Averaged Birds in room_3 Daily Time Budget

day_flat <- cbind(day_tbs_df[c(1:2,6)], stack(day_tbs_df[3:5]))

day_flat$ind <- factor(day_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(day_flat$interval1),n=1),origin="1970-01-01")),
as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(day_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")), by="1 days")




# room_3_bp <- ggplot(day_flat, aes(x = factor(as.Date(interval1)), y=values, fill=ind)) + 
#   geom_boxplot()

# ggsave(paste0("../figures/all_day/day_daily_time_budget_boxplot_for_room_3",".png"), room_3_bp)

# Averaged Birds in room_3 Nightly Time Budget

night_flat <- cbind(night_tbs_df[c(1:2,6)], stack(night_tbs_df[3:5]))

night_flat$ind <- factor(night_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(night_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1)),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(night_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")), by="1 days")


y_lim <- length(unique(night_flat$tagname))+.001

room_3_sb_night_plot <- ggplot(data = night_flat, aes(x = as.Date(as.POSIXct.numeric(as.numeric(interval1),origin="1970-01-01")), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= as.Date(datebreaks), minor_breaks=as.Date(all_datebreak)) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Night for Room 3") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))

ggsave(paste0("../figures/all_day/room3/night_daily_time_budget_stack_bar_for_room_3",".png"), room_3_sb_night_plot)


room_3_sb_day_plot <- ggplot(data = day_flat, aes(x = as.Date(ymd_hms(as.numeric(interval1))), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= as.Date(datebreaks), minor_breaks=as.Date(all_datebreak)) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Day for Room 3") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))

ggsave(paste0("../figures/all_day/room3/day_daily_time_budget_stack_bar_for_room_3",".png"), room_3_sb_day_plot)

### Save Overall Interval, Daily Interval and Nightly Interval to disk ###

# Overall

for(i in 1:length( room_3_interval$tagname)){

    current_tag <- room_3_interval$tagname[i]

    current_tsibble <- room_3_interval |>
        slice(i) |> 
        pull(interval) |> 
        pluck(1)

    current_tsibble$tagname <- rep(current_tag, length(current_tsibble$t1))

    write.csv(current_tsibble, paste0("../intermediate/all_rooms/overall_interval/room_3_interval_",current_tag,".csv"),row.names=F)
}

# Daily 

room_3_day_int <- room_3_all_room_day |>
  mutate(daily_int = map(day, ~time_to_intervals(.x)))

for(i in 1:length(room_3_day_int$tagname)){

    current_tag <- room_3_day_int$tagname[i]

    current_day_tsibble <- room_3_day_int |>
        slice(i) |> 
        pull(daily_int) |> 
        pluck(1)
    
    current_day_tsibble$tagname <- rep(current_tag, length(current_day_tsibble$t1))
  
    write.csv(current_day_tsibble, paste0("../intermediate/all_rooms/daily_interval/room_3_interval_",current_tag,".csv"),row.names=F)
}

# Nightly

room_3_night_int <- room_3_all_room_day |>
  mutate(nightly_int = map(night, ~time_to_intervals(.x)))

for(i in 1:length(room_3_night_int$tagname)){

    current_tag <- room_3_night_int$tagname[i]

    current_night_tsibble <- room_3_night_int |>
        slice(i) |> 
        pull(nightly_int) |> 
        pluck(1)
  
    current_night_tsibble$tagname <- rep(current_tag, length(current_night_tsibble$t1))

    write.csv(current_night_tsibble, paste0("../intermediate/all_rooms/nightly_interval/room_3_interval_",current_tag,".csv"),row.names=F)
}

### END OF ROOM 3 ANALYSIS ###
