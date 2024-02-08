#### All Room: 2,3,8,11 Joint analysis ####

source("./rfid_functions.R")

# Generate Transition tables from Room 11
library(xts)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tsibble)
library(testthat)
library(cluster)

# read in room 2 tables
library(readr)
library(ggplot2)

rm_2_overall_int <- Sys.glob("../intermediate/all_rooms/overall_interval/room_2_interval_*")

rm_2_day_int <- Sys.glob("../intermediate/all_rooms/daily_interval/room_2_interval_*")

rm_2_night_int <- Sys.glob("../intermediate/all_rooms/nightly_interval/room_2_interval_*")

rm_2_overall_df <- read_csv(rm_2_overall_int)

rm_2_nest_overall_int <- rm_2_overall_df |>
  nest(data = -tagname)

rm_2_day_int_df <- read_csv(rm_2_day_int)

rm_2_nest_day_int <- rm_2_day_int_df |>
  nest(data = -tagname)

rm_2_night_int_df <- read_csv(rm_2_night_int)

rm_2_nest_night_int <- rm_2_night_int_df |>
   nest(data = - tagname)

# calc num trans in room 2

rm_2_overall <- rm_2_nest_overall_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('overall summary: '))
print(summary(rm_2_overall$ntrans))

rm_2_day <- rm_2_nest_day_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('day summary: '))
print(summary(rm_2_day$ntrans))


rm_2_night <- rm_2_nest_night_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('night summary: '))
print(summary(rm_2_night$ntrans))

summary_rm_2 <- summary(rm_2_overall$ntrans)

l2m <- summary_rm_2[2]

m2h <- summary_rm_2[5]

rm_2_low_act <- rm_2_overall[rm_2_overall$ntrans <= l2m,]
rm_2_low_act$activity <- rep("low",length(rm_2_low_act$ntrans))

rm_2_med_act <- rm_2_overall[(l2m < rm_2_overall$ntrans) & (rm_2_overall$ntrans <= m2h),]
rm_2_med_act$activity <- rep("medium",length(rm_2_med_act$ntrans))

rm_2_high_act <- rm_2_overall[(rm_2_overall$ntrans > m2h),]
rm_2_high_act$activity <- rep("high",length(rm_2_high_act$ntrans))

rm_2_org_overall <- bind_rows(rm_2_low_act,rm_2_med_act, rm_2_high_act)

write.csv(rm_2_org_overall, "../intermediate/rm_2_activity_class.csv", row.names=F)

expect_equal(length(rm_2_org_overall$ntrans), length(rm_2_overall$ntrans))



# # calc num trans room 3

rm_3_overall_int <- Sys.glob("../intermediate/all_rooms/overall_interval/room_3_interval_*")

rm_3_day_int <- Sys.glob("../intermediate/all_rooms/daily_interval/room_3_interval_*")

rm_3_night_int <- Sys.glob("../intermediate/all_rooms/nightly_interval/room_3_interval_*")

rm_3_overall_df <- read_csv(rm_3_overall_int)

rm_3_nest_overall_int <- rm_3_overall_df |>
  nest(data = -tagname)

rm_3_day_int_df <- read_csv(rm_3_day_int)

rm_3_nest_day_int <- rm_3_day_int_df |>
  nest(data = -tagname)

rm_3_night_int_df <- read_csv(rm_3_night_int)

rm_3_nest_night_int <- rm_3_night_int_df |>
   nest(data = - tagname)

rm_3_overall <- rm_3_nest_overall_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('overall summary: '))
print(summary(rm_3_overall$ntrans))

rm_3_day <- rm_3_nest_day_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('day summary: '))
print(summary(rm_3_day$ntrans))


rm_3_night <- rm_3_nest_night_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('night summary: '))
print(summary(rm_3_night$ntrans))

summary_rm_3 <- summary(rm_3_overall$ntrans)

l2m <- summary_rm_3[2]

m2h <- summary_rm_3[5]

rm_3_low_act <- rm_3_overall[rm_3_overall$ntrans <= l2m,]
rm_3_low_act$activity <- rep("low",length(rm_3_low_act$ntrans))

rm_3_med_act <- rm_3_overall[(l2m < rm_3_overall$ntrans) & (rm_3_overall$ntrans <= m2h),]
rm_3_med_act$activity <- rep("medium",length(rm_3_med_act$ntrans))

rm_3_high_act <- rm_3_overall[(rm_3_overall$ntrans > m2h),]
rm_3_high_act$activity <- rep("high",length(rm_3_high_act$ntrans))

rm_3_org_overall <- bind_rows(rm_3_low_act,rm_3_med_act, rm_3_high_act)

write.csv(rm_3_org_overall, "../intermediate/rm_3_activity_class.csv", row.names=F)

expect_equal(length(rm_3_org_overall$ntrans), length(rm_3_overall$ntrans))

# # calc num trans room 8

rm_8_overall_int <- Sys.glob("../intermediate/all_rooms/overall_interval/room_8_interval_*")

rm_8_day_int <- Sys.glob("../intermediate/all_rooms/daily_interval/room_8_interval_*")

rm_8_night_int <- Sys.glob("../intermediate/all_rooms/nightly_interval/room_8_interval_*")

rm_8_overall_df <- read_csv(rm_8_overall_int)

rm_8_nest_overall_int <- rm_8_overall_df |>
  nest(data = -tagname)

rm_8_day_int_df <- read_csv(rm_8_day_int)

rm_8_nest_day_int <- rm_8_day_int_df |>
  nest(data = -tagname)

rm_8_night_int_df <- read_csv(rm_8_night_int)

rm_8_nest_night_int <- rm_8_night_int_df |>
   nest(data = - tagname)

rm_8_overall <- rm_8_nest_overall_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('overall summary: '))
print(summary(rm_8_overall$ntrans))

rm_8_day <- rm_8_nest_day_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('day summary: '))
print(summary(rm_8_day$ntrans))


rm_8_night <- rm_8_nest_night_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('night summary: '))
print(summary(rm_8_night$ntrans))

summary_rm_8 <- summary(rm_8_overall$ntrans)

l2m <- summary_rm_8[2]

m2h <- summary_rm_8[5]

rm_8_low_act <- rm_8_overall[rm_8_overall$ntrans <= l2m,]
rm_8_low_act$activity <- rep("low",length(rm_8_low_act$ntrans))

rm_8_med_act <- rm_8_overall[(l2m < rm_8_overall$ntrans) & (rm_8_overall$ntrans <= m2h),]
rm_8_med_act$activity <- rep("medium",length(rm_8_med_act$ntrans))

rm_8_high_act <- rm_8_overall[(rm_8_overall$ntrans > m2h),]
rm_8_high_act$activity <- rep("high",length(rm_8_high_act$ntrans))

rm_8_org_overall <- bind_rows(rm_8_low_act,rm_8_med_act, rm_8_high_act)

write.csv(rm_8_org_overall, "../intermediate/rm_8_activity_class.csv", row.names=F)

expect_equal(length(rm_8_org_overall$ntrans), length(rm_8_overall$ntrans))

# # calc num trans room 11

rm_11_overall_int <- Sys.glob("../intermediate/all_rooms/overall_interval/room_11_interval_*")

rm_11_day_int <- Sys.glob("../intermediate/all_rooms/daily_interval/room_11_interval_*")

rm_11_night_int <- Sys.glob("../intermediate/all_rooms/nightly_interval/room_11_interval_*")

rm_11_overall_df <- read_csv(rm_11_overall_int)

rm_11_nest_overall_int <- rm_11_overall_df |>
  nest(data = -tagname)

rm_11_day_int_df <- read_csv(rm_11_day_int)

rm_11_nest_day_int <- rm_11_day_int_df |>
  nest(data = -tagname)

rm_11_night_int_df <- read_csv(rm_11_night_int)

rm_11_nest_night_int <- rm_11_night_int_df |>
   nest(data = - tagname)

rm_11_overall <- rm_11_nest_overall_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('overall summary: '))
print(summary(rm_11_overall$ntrans))

rm_11_day <- rm_11_nest_day_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('day summary: '))
print(summary(rm_11_day$ntrans))


rm_11_night <- rm_11_nest_night_int |>
    mutate(ntrans = map(data, ~length(.x$t1)-1)) |>
    select(c(tagname,ntrans)) |>
    unnest(ntrans)

print(paste('night summary: '))
print(summary(rm_11_night$ntrans))

summary_rm_11 <- summary(rm_11_overall$ntrans)

l2m <- summary_rm_11[2]

m2h <- summary_rm_11[5]

rm_11_low_act <- rm_11_overall[rm_11_overall$ntrans <= l2m,]
rm_11_low_act$activity <- rep("low",length(rm_11_low_act$ntrans))

rm_11_med_act <- rm_11_overall[(l2m < rm_11_overall$ntrans) & (rm_11_overall$ntrans <= m2h),]
rm_11_med_act$activity <- rep("medium",length(rm_11_med_act$ntrans))

rm_11_high_act <- rm_11_overall[(rm_11_overall$ntrans > m2h),]
rm_11_high_act$activity <- rep("high",length(rm_11_high_act$ntrans))

rm_11_org_overall <- bind_rows(rm_11_low_act,rm_11_med_act, rm_11_high_act)

write.csv(rm_11_org_overall, "../intermediate/rm_11_activity_class.csv", row.names=F)

expect_equal(length(rm_11_org_overall$ntrans), length(rm_11_overall$ntrans))

# # concat all num trans tables

rm_2_overall$rm <- 2

rm_3_overall$rm <- 3

rm_8_overall$rm <- 8

rm_11_overall$rm <- 11

overall_table <- bind_rows(rm_2_overall, rm_3_overall, rm_8_overall, rm_11_overall)


# # separate out low medium high activity

summary_overall <- summary(overall_table$ntrans)

l2m <- summary_overall[2]

m2h <- summary_overall[5]

overall_low_act <- overall_table[overall_table$ntrans <= l2m,]
overall_low_act$activity <- rep("low",length(overall_low_act$ntrans))

overall_med_act <- overall_table[(l2m < overall_table$ntrans) & (overall_table$ntrans <= m2h),]
overall_med_act$activity <- rep("medium",length(overall_med_act$ntrans))

overall_high_act <- overall_table[(overall_table$ntrans > m2h),]
overall_high_act$activity <- rep("high",length(overall_high_act$ntrans))

overall_org_table <- bind_rows(overall_low_act,overall_med_act, overall_high_act)

overall_org_table <- overall_org_table[order(overall_org_table$ntrans),]

write.csv(overall_org_table, "../intermediate/all_rooms/overall_org_table.csv", row.names=FALSE)

### Compare overall low med high activity to day time classification ###

rm_2_day$rm <- 2

rm_3_day$rm <- 3

rm_8_day$rm <- 8

rm_11_day$rm <- 11

day_table <- bind_rows(rm_2_day, rm_3_day, rm_8_day, rm_11_day)

summary_day <- summary(day_table$ntrans)

l2m <- summary_day[2]

m2h <- summary_day[5]

day_low_act <- day_table[day_table$ntrans <= l2m,]
day_low_act$activity <- rep("low",length(day_low_act$ntrans))

day_med_act <- day_table[(l2m < day_table$ntrans) & (day_table$ntrans <= m2h),]
day_med_act$activity <- rep("medium",length(day_med_act$ntrans))

day_high_act <- day_table[(day_table$ntrans > m2h),]
day_high_act$activity <- rep("high",length(day_high_act$ntrans))

day_org_table <- bind_rows(day_low_act,day_med_act, day_high_act)

day_org_table <- day_org_table[order(day_org_table$ntrans),]

sorted_overall_org <- overall_org_table[order(overall_org_table$activity, overall_org_table$tagname),]

sorted_day_org <- day_org_table[order(day_org_table$activity, day_org_table$tagname),]

# Differences

(diff <- setdiff( sorted_overall_org[,c(1,4)], sorted_day_org[,c(1,4)]))
same <- intersect( sorted_overall_org[,c(1,4)], sorted_day_org[,c(1,4)])

# where they are in overall
(overall_org_table[overall_org_table$tagname %in% diff$tagname,])
# where they are in day
(day_org_table[day_org_table$tagname %in% diff$tagname,])

### Compare overall low med high act to night time classification ###

rm_2_night$rm <- 2

rm_3_night$rm <- 3

rm_8_night$rm <- 8

rm_11_night$rm <- 11

night_table <- bind_rows(rm_2_night, rm_3_night, rm_8_night, rm_11_night)

summary_night <- summary(night_table$ntrans)

l2m <- summary_night[2]

m2h <- summary_night[5]

night_low_act <- night_table[night_table$ntrans <= l2m,]
night_low_act$activity <- rep("low",length(night_low_act$ntrans))

night_med_act <- night_table[(l2m < night_table$ntrans) & (night_table$ntrans <= m2h),]
night_med_act$activity <- rep("medium",length(night_med_act$ntrans))

night_high_act <- night_table[(night_table$ntrans > m2h),]
night_high_act$activity <- rep("high",length(night_high_act$ntrans))

night_org_table <- bind_rows(night_low_act,night_med_act, night_high_act)

night_org_table <- night_org_table[order(night_org_table$ntrans),]

sorted_overall_org <- overall_org_table[order(overall_org_table$activity, overall_org_table$tagname),]

sorted_night_org <- night_org_table[order(night_org_table$activity, night_org_table$tagname),]

n_activity <- data.frame(table(sorted_day_org$activity))
total <- sum(n_activity$Freq)
n_activity$Var1 <- as.character(n_activity$Var1)
n_activity[nrow(n_activity) + 1,] = c("total",total)
n_activity <- data.frame(t(n_activity))
colnames(n_activity) <- n_activity[1,]
n_activity <- n_activity %>% select(low,medium,high,total)
n_activity <- n_activity[2,]

write.csv(n_activity, "../intermediate/n_classified_activity.csv")



# Differences

(diff <- setdiff( sorted_overall_org[,c(1,4)], sorted_night_org[,c(1,4)]))
same <- intersect( sorted_overall_org[,c(1,4)], sorted_night_org[,c(1,4)])

# where they are in overall
(overall_org_table[overall_org_table$tagname %in% diff$tagname,])
# where they are in day
(night_org_table[night_org_table$tagname %in% diff$tagname,])


### Where do the high low medium birds nest at night? ### 



# read in time budget tables

rm_2_day_tb <- Sys.glob("../intermediate/all_rooms/room_2_day_time_budget_*")
rm_2_night_tb <- Sys.glob("../intermediate/all_rooms/room_2_night_time_budget_*")

rm_3_day_tb <- Sys.glob("../intermediate/all_rooms/room_3_day_time_budget_*")
rm_3_night_tb <- Sys.glob("../intermediate/all_rooms/room_3_night_time_budget_*")

rm_8_day_tb <- Sys.glob("../intermediate/all_rooms/room_8_day_time_budget_*")
rm_8_night_tb <- Sys.glob("../intermediate/all_rooms/room_8_night_time_budget_*")

rm_11_day_tb <- Sys.glob("../intermediate/all_rooms/room_11_day_time_budget_*")
rm_11_night_tb <- Sys.glob("../intermediate/all_rooms/room_11_night_time_budget_*")

day_tb_df <- read_csv(c(rm_2_day_tb, rm_3_day_tb, rm_8_day_tb, rm_11_day_tb))

nest_day_tb <- day_tb_df |>
  nest(data = -tagname)

night_tb_df <- read_csv(c(rm_2_night_tb, rm_3_night_tb, rm_8_night_tb, rm_11_night_tb))

nest_night_tb <- night_tb_df |>
   nest(data = - tagname)

# select high activity birds
overall_high <- overall_org_table[overall_org_table$activity == "high",]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_high$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))

# select med activity birds
overall_med <- overall_org_table[overall_org_table$activity == "medium",]

# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_med$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))

# select low activity birds
overall_low <- overall_org_table[overall_org_table$activity == "low",]

# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_low$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))



low_tab <- colSums(table(low_act_nest[,c(1,3)])[,c("Bottom","Middle","Top")])
med_tab <- colSums(table(med_act_nest[,c(1,3)])[,c("Bottom","Middle","Top")])
high_tab <- colSums(table(high_act_nest[,c(1,3)])[,c("Bottom","Middle","Top")])

low_tot <- sum(low_tab)
med_tot <- sum(med_tab)
high_tot <- sum(high_tab)
tot_tot <- sum(low_tot,med_tot,high_tot)

absolute_nest <- data.frame(rbind(low_tab, med_tab, high_tab))
rownames(absolute_nest) <- c("low","medium","high")
absolute_nest$n <- c(low_tot,med_tot,high_tot)

write.csv(absolute_nest, "../intermediate/absolute_nest.csv")

relative_nest <- data.frame(rbind(low_tab/low_tot, med_tab/med_tot, high_tab/high_tot))
rownames(relative_nest) <- c("low","medium","high")
relative_nest$n <- c(low_tot,med_tot,high_tot)

write.csv(relative_nest, "../intermediate/relative_nest.csv")

### END Activity classification with overall table ###


### Activity Classification of room 2 ###

rm_2 <- overall_table[overall_table$rm == 2,]

summary_rm_2 <- summary(rm_2$ntrans)

l2m <- summary_rm_2[2]

m2h <- summary_rm_2[5]

rm_2_low_act <- rm_2[rm_2$ntrans <= l2m,]
rm_2_low_act$activity <- rep("low",length(rm_2_low_act$ntrans))

rm_2_med_act <- rm_2[(l2m < rm_2$ntrans) & (rm_2$ntrans <= m2h),]
rm_2_med_act$activity <- rep("medium",length(rm_2_med_act$ntrans))

rm_2_high_act <- rm_2[(rm_2$ntrans > m2h),]
rm_2_high_act$activity <- rep("high",length(rm_2_high_act$ntrans))

rm_2_org_table <- bind_rows(rm_2_low_act,rm_2_med_act, rm_2_high_act)

rm_2_org_table <- rm_2_org_table[order(rm_2_org_table$ntrans),]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_2_high_act$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_2_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_2_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))

### END Activity Class of room 2 ###

### Activity Classification of room 3 ###

rm_3 <- overall_table[overall_table$rm == 3,]

summary_rm_3 <- summary(rm_3$ntrans)

l2m <- summary_rm_3[2]

m2h <- summary_rm_3[5]

rm_3_low_act <- rm_3[rm_3$ntrans <= l2m,]
rm_3_low_act$activity <- rep("low",length(rm_3_low_act$ntrans))

rm_3_med_act <- rm_3[(l2m < rm_3$ntrans) & (rm_3$ntrans <= m2h),]
rm_3_med_act$activity <- rep("medium",length(rm_3_med_act$ntrans))

rm_3_high_act <- rm_3[(rm_3$ntrans > m2h),]
rm_3_high_act$activity <- rep("high",length(rm_3_high_act$ntrans))

rm_3_org_table <- bind_rows(rm_3_low_act,rm_3_med_act, rm_3_high_act)

rm_3_org_table <- rm_3_org_table[order(rm_3_org_table$ntrans),]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_3_high_act$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_3_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_3_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))

### END Activity Class of room 3 ###

### Activity Classification of room 8 ###

rm_8 <- overall_table[overall_table$rm == 8,]

summary_rm_8 <- summary(rm_8$ntrans)

l2m <- summary_rm_8[2]

m2h <- summary_rm_8[5]

rm_8_low_act <- rm_8[rm_8$ntrans <= l2m,]
rm_8_low_act$activity <- rep("low",length(rm_8_low_act$ntrans))

rm_8_med_act <- rm_8[(l2m < rm_8$ntrans) & (rm_8$ntrans <= m2h),]
rm_8_med_act$activity <- rep("medium",length(rm_8_med_act$ntrans))

rm_8_high_act <- rm_8[(rm_8$ntrans > m2h),]
rm_8_high_act$activity <- rep("high",length(rm_8_high_act$ntrans))

rm_8_org_table <- bind_rows(rm_8_low_act,rm_8_med_act, rm_8_high_act)

rm_8_org_table <- rm_8_org_table[order(rm_8_org_table$ntrans),]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_8_high_act$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_8_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_8_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))

### END Activity Class of room 8 ###

### Activity Classification of room 11 ###

rm_11 <- overall_table[overall_table$rm == 11,]

summary_rm_11 <- summary(rm_11$ntrans)

l2m <- summary_rm_11[2]

m2h <- summary_rm_11[5]

rm_11_low_act <- rm_11[rm_11$ntrans <= l2m,]
rm_11_low_act$activity <- rep("low",length(rm_11_low_act$ntrans))

rm_11_med_act <- rm_11[(l2m < rm_11$ntrans) & (rm_11$ntrans <= m2h),]
rm_11_med_act$activity <- rep("medium",length(rm_11_med_act$ntrans))

rm_11_high_act <- rm_11[(rm_11$ntrans > m2h),]
rm_11_high_act$activity <- rep("high",length(rm_11_high_act$ntrans))

rm_11_org_table <- bind_rows(rm_11_low_act,rm_11_med_act, rm_11_high_act)

rm_11_org_table <- rm_11_org_table[order(rm_11_org_table$ntrans),]

# select high time budgets based on ids

high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_11_high_act$tagname,]

high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_11_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_11_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~nightZoneFromTB(.x))) |>
  unnest(nest)

print("Where do the low activity birds nest at night: ")
(sort(table(low_act_nest$nest),decreasing=T))

### END Activity Class of room 11 ###

### END Activity Class for each room ### 



### does the weekly time budget differ from feb to april ### 

library(emmeans)
library(lme4)
library(lmerTest)

id_lookup <- unique(overall_org_table[,c(1,3)])

day_tb_df$week <- week(day_tb_df$interval1)

day_bottom_sum <- day_tb_df |>
  group_by(tagname,week) |>
  summarize(bottom_mean = mean(Bottom)) 

day_middle_sum <- day_tb_df |>
  group_by(tagname,week) |>
  summarize(middle_mean = mean(Middle)) 

day_top_sum <- day_tb_df |>
  group_by(tagname,week) |>
  summarize(top_mean = mean(Top))

overall_day_summary <- cbind(day_bottom_sum,day_middle_sum[,3],day_top_sum[,3])
overall_day_summary <- data.frame(merge(overall_day_summary,overall_org_table[,c(1,4)], by="tagname"))
overall_day_summary$activity <- factor(overall_day_summary$activity, levels=c("low","medium","high"))
overall_day_summary$weekFac <- factor(overall_day_summary$week)
overall_day_summary$rm <- id_lookup[match(overall_day_summary$tagname, id_lookup$tagname),2]$rm
(unique(day_tb_df$interval1))
(unique(day_tb_df$interval2))

### BEGINING OF BOTTOM ZONE ANALYSIS ###



m1 <- lmer(bottom_mean ~ weekFac + activity + weekFac:activity + (1|tagname) + (1|rm), overall_day_summary)
summary(m1)
anova(m1)
m1.res <- resid(m1)

# generate and save residual plot of model to check assumptions

png("../figures/all_day/model_diag/bottom_mean_resid.png")
plot(fitted(m1),m1.res)
abline(0,0)
dev.off()

# generate and save Q-Q normal plot to check assumptions

png("../figures/all_day/model_diag/bottom_mean_qq.png")
qqnorm(m1.res)
qqline(m1.res)
dev.off()


# Get estimations of bottom time spent

m1.bottom.means <- emmeans(m1, specs=list(weekMeans = ~weekFac,
actMeans = ~activity,
jointMeans=~weekFac:activity))

 
# interaction plot of week on x
png("../figures/all_day/model_diag/bottom_interaction_act_week.png")
emmip(m1.bottom.means$jointMeans, activity~weekFac) + 
xlab("Age (Weeks)") + ylab("Predicted Proportion of Time in Zone") + 
scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
scale_color_manual(values=c("#619CFF","#F8766D","#00BA38"))

dev.off()

# interaction plot of activity on x
png("../figures/all_day/model_diag/bottom_interaction_week_act.png")
emmip(m1.bottom.means$jointMeans, weekFac~activity)
dev.off()

# Insufficient evidence of interaction 

# does the mean time spent in the bottom zone differ between bird activity levels?
contrast(m1.bottom.means$actMean, method=list(
  low.vs.med.high = c(-2,1,1)/2,
  med.vs.high = c(0,-1,1)
))


# contrast to decode how mean time in bottom zone differes in birds

contrast(m1.bottom.means$weekMeans, method=list(
  first.vs.last = c(-1,0,0,0,0,0,0,0,1),
  fhalf.vs.lhalf = c(-1,-1,-1,-1,0,1,1,1,1)), adjust="bonferroni")

# Examine if there is a linear/non-linear effect of week on time spent in bottom zone

contrast(m1.bottom.means$weekMeans,"poly")[1]
test(contrast(m1.bottom.means$weekMeans,"poly")[2:6],joint=TRUE)

contrast(m1.bottom.means$actMeans,"poly")[1]
contrast(m1.bottom.means$actMeans,"poly")[2]

### END OF BOTTOM ZONE ANALYSIS ###

### BEGINING OF MIDDLE ZONE ANALYSIS ###



m2 <- lmer(middle_mean ~ weekFac + activity + weekFac:activity + (1|tagname)+(1|rm), overall_day_summary)
summary(m2)
anova(m2)
m2.res <- resid(m2)

# generate and save residual plot of model to check assumptions

png("../figures/all_day/model_diag/middle_mean_resid.png")
plot(fitted(m2),m2.res)
abline(0,0)
dev.off()

# generate and save Q-Q normal plot to check assumptions

png("../figures/all_day/model_diag/middle_mean_qq.png")
qqnorm(m2.res)
qqline(m2.res)
dev.off()


# Get estimations of middle time spent

m2.middle.means <- emmeans(m2, specs=list(weekMeans = ~weekFac,
actMeans = ~activity,
jointMeans=~weekFac:activity))

 
# interaction plot of week on x
png("../figures/all_day/model_diag/middle_interaction_act_week.png")
emmip(m2.middle.means$jointMeans, activity~weekFac) + 
xlab("Age (Weeks)") + ylab("Predicted Proportion of Time in Zone") + 
scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
scale_color_manual(values=c("#619CFF","#F8766D","#00BA38"))
dev.off()

# interaction plot of activity on x
png("../figures/all_day/model_diag/middle_interaction_week_act.png")
emmip(m2.middle.means$jointMeans, weekFac~activity)
dev.off()


# I don't really see evidence of interaction so we could use marginal means


# does the mean time spent in the middle zone differ between bird activity levels?
contrast(m2.middle.means$actMean, method=list(
  low.vs.med.high = c(-2,1,1)/2,
  med.vs.high = c(0,-1,1)
))


# contrast to decode how mean time in bottom zone differes in birds

contrast(m2.middle.means$weekMeans, method=list(
  first.vs.last = c(-1,0,0,0,0,0,0,0,1),
  fhalf.vs.lhalf = c(-1,-1,-1,-1,0,1,1,1,1)), adjust="bonferroni")


# Does the mean time spent in the middle zone have a linear effect?
contrast(m2.middle.means$weekMeans,"poly")[1]

# Does the mean time spent in the middle zone have a non-linear effect?
test(contrast(m2.middle.means$weekMeans,"poly")[2:6],joint=TRUE)

### END OF MIDDLE ZONE ANALYSIS ###

### BEGINING OF TOP ZONE ANALYSIS ###


m3 <- lmer(top_mean ~ weekFac + activity + weekFac:activity + (1|tagname) + (1|rm), overall_day_summary)
summary(m3)
anova(m3)
m3.res <- resid(m3)

# generate and save residual plot of model to check assumptions

png("../figures/all_day/model_diag/top_mean_resid.png")
plot(fitted(m3),m3.res)
abline(0,0)
dev.off()

# generate and save Q-Q normal plot to check assumptions

png("../figures/all_day/model_diag/top_mean_qq.png")
qqnorm(m3.res)
qqline(m3.res)
dev.off()


# Get estimations of middle time spent

m3.top.means <- emmeans(m3, specs=list(weekMeans = ~weekFac,
actMeans = ~activity,
jointMeans=~weekFac:activity))

 
# interaction plot of week on x
png("../figures/all_day/model_diag/top_interaction_act_week.png")
emmip(m3.top.means$jointMeans, activity~weekFac)+ 
xlab("Age (Weeks)") + ylab("Predicted Proportion of Time in Zone") + 
scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
scale_color_manual(values=c("#619CFF","#F8766D","#00BA38"))
dev.off()

# interaction plot of activity on x
png("../figures/all_day/model_diag/top_interaction_week_act.png")
emmip(m3.top.means$jointMeans, weekFac~activity)
dev.off()


# Some evidence of interaction week 13 to 16


# does the mean time spent in the top zone differ between bird activity levels?
contrast(m3.top.means$actMean, method=list(
  low.vs.med.high = c(-2,1,1)/2,
  med.vs.high = c(0,-1,1)
))


# contrast to decode how mean time in top zone differes in birds

contrast(m3.top.means$weekMeans, method=list(
  first.vs.last = c(-1,0,0,0,0,0,0,0,1),
  fhalf.vs.lhalf = c(-1,-1,-1,-1,0,1,1,1,1)), adjust="bonferroni")


# Does the mean time spent in the top zone have a linear effect?
contrast(m3.top.means$weekMeans,"poly")[1]

# Does the mean time spent in the top zone have a non-linear effect?
test(contrast(m3.top.means$weekMeans,"poly")[2:6],joint=TRUE)

### END OF TOP ZONE ANALYSIS ###

### End weekly time budget differ march to april ###

### Does Keel Score differ based on activity level ###

#TODO print N's for keel score in activity level

keel_score <- read_csv('../data/keel_score/rfid_keel_scores_old_classification.csv')

# join keel score to overall day summary table

unique(overall_day_summary$tagname) %in% keel_score$tag


overall_day_summary_w_keel <- data.frame(merge(overall_day_summary,keel_score[,c(1,4)], by.x="tagname",by.y="tag"))

overall_day_summary_w_keel <- overall_day_summary_w_keel[overall_day_summary_w_keel$keel_score != ".",]

overall_day_summary_w_keel$keel_score <- as.numeric(overall_day_summary_w_keel$keel_score)

print("Tagnames Obeserved with Keel Score: ")
print(unique(overall_day_summary_w_keel$tagname))


raw_n_keel <-  data.frame(table(unique(overall_day_summary_w_keel[,c(1,6,9)])))
(sum_n_keel <- raw_n_keel %>% group_by(activity,keel_score) %>%  summarise(n = sum(Freq)))

write.csv(sum_n_keel, "../intermediate/n_keel.csv", row.names=F)

# TODO update these to be better named
tmp2 <- unique(overall_day_summary_w_keel %>% select(c(tagname,activity,rm,keel_score)))
tmp2$tagname <- factor(tmp2$tagname,levels=tmp2$tagname)
tmp2$tag <- as.numeric(tmp2$tagname)
m4 <- lmer(keel_score ~ activity + (1|rm), tmp2)
summary(m4)
anova(m4)

m4_means <- emmeans(m4, specs=list( actMean = ~activity))

c1 <- c(-1,.5,.5)
c2 <- c(0,-1,1)

contrast(m4_means, method=list(low.vs.medHigh = c1,
med.vs.high = c2),adjust="bonferroni")

png("../figures/all_day/model_diag/keel_score_mean_m4.png",width=5,height=3, units='in',res=300)
plot(m4_means, horizontal = F,comparisons=T)
dev.off()

m5 <- aov(keel_score ~ activity, overall_day_summary_w_keel)
summary(m5)

m5_means <- emmeans(m5, specs=list( actMean = ~activity))

c1 <- c(-1,.5,.5)
c2 <- c(0,-1,1)

contrast(m5_means, method=list(low.vs.medHigh = c1,
med.vs.high = c2),adjust="bonferroni")

png("../figures/all_day/model_diag/keel_score_mean.png",width=5,height=3, units='in',res=300)
plot(m5_means, horizontal = F,comparisons=T)
dev.off()

### End does Keel Score Differ based on activity level ###

### Make Low Activity Daily Time Budget Plot ###

# day_tbs_df
# # A tibble: 580 × 6
#    interval1           interval2             Bottom  Middle    Top tagname
#    <dttm>              <dttm>                 <dbl>   <dbl>  <dbl>   <dbl>
#  1 2021-03-10 04:00:00 2021-03-10 21:59:55 0        0.501   0.499     6925
#  2 2021-03-11 04:00:00 2021-03-11 21:59:55 0        0.324   0.676     6925
#  3 2021-03-12 04:00:00 2021-03-12 21:59:55 0        0.226   0.774     6925
#  4 2021-03-13 04:00:00 2021-03-13 21:59:55 0        0.267   0.733     6925
#  5 2021-03-14 04:00:00 2021-03-14 21:59:55 0        0.594   0.406     6925

# Select the overall low activity bird ids

low_activity_ids <- overall_low_act$tagname

# read in the overall time budgets from low activity birds

low_day_tb <- Sys.glob(paste0("../intermediate/all_rooms/*day_*",low_activity_ids,".csv"))
low_day_df <- read_csv(low_day_tb)

day_flat <- cbind(low_day_df[c(1:2,6)], stack(low_day_df[3:5]))

day_flat$ind <- factor(day_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="1 days")

y_lim <- length(unique(day_flat$tagname))+.001

low_day_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Day for Low Activity Birds (n=10)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))

#TODO send regmi this figure
ggsave(paste0("../figures/all_day/all_rooms/day_daily_time_budget_stack_bar_for_low_act",".png"), low_day_sb_plot,width = 5, height = 3, units = "in")

### End Low Activity Daily Time Budget Plot ###

### Make Medium Activity Daily Time Budget Plot ###

# Select the overall low activity bird ids

# n = 18
med_activity_ids <- overall_med_act$tagname

# read in the overall time budgets from low activity birds

med_day_tb <- Sys.glob(paste0("../intermediate/all_rooms/*day_*",med_activity_ids,".csv"))
med_day_df <- read_csv(med_day_tb)

day_flat <- cbind(med_day_df[c(1:2,6)], stack(med_day_df[3:5]))

day_flat$ind <- factor(day_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="1 days")

y_lim <- length(unique(day_flat$tagname))+.001

med_day_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Day for Medium Activity Birds (n=18)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/day_daily_time_budget_stack_bar_for_med_act",".png"), med_day_sb_plot,width = 5, height = 3, units = "in")

### End Medium Activity Daily Time Budget Plot ###

### Make High Activity Daily Time Budget Plot ###

# Select the overall low activity bird ids

# n = 9
high_activity_ids <- overall_high_act$tagname

# read in the overall time budgets from low activity birds

high_day_tb <- Sys.glob(paste0("../intermediate/all_rooms/*day_*",high_activity_ids,".csv"))
high_day_df <- read_csv(high_day_tb)

day_flat <- cbind(high_day_df[c(1:2,6)], stack(high_day_df[3:5]))

day_flat$ind <- factor(day_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(day_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(day_flat$interval1),n=1))), by="1 days")

y_lim <- length(unique(day_flat$tagname))+.001

high_day_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Day of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Day for High Activity Birds (n=9)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/day_daily_time_budget_stack_bar_for_high_act",".png"), high_day_sb_plot,width = 5, height = 3, units = "in")

### End High Activity Daily Time Budget Plot ###

### Make Low Activity Nightly Time Budget Plot ###

# day_tbs_df
# # A tibble: 580 × 6
#    interval1           interval2             Bottom  Middle    Top tagname
#    <dttm>              <dttm>                 <dbl>   <dbl>  <dbl>   <dbl>
#  1 2021-03-10 04:00:00 2021-03-10 21:59:55 0        0.501   0.499     6925
#  2 2021-03-11 04:00:00 2021-03-11 21:59:55 0        0.324   0.676     6925
#  3 2021-03-12 04:00:00 2021-03-12 21:59:55 0        0.226   0.774     6925
#  4 2021-03-13 04:00:00 2021-03-13 21:59:55 0        0.267   0.733     6925
#  5 2021-03-14 04:00:00 2021-03-14 21:59:55 0        0.594   0.406     6925

# Select the overall low activity bird ids

low_activity_ids <- overall_low_act$tagname

# read in the overall time budgets from low activity birds

low_night_tb <- Sys.glob(paste0("../intermediate/all_rooms/*night_*",low_activity_ids,".csv"))
low_night_df <- read_csv(low_night_tb)

night_flat <- cbind(low_night_df[c(1:2,6)], stack(low_night_df[3:5]))

night_flat$ind <- factor(night_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="1 days")

y_lim <- length(unique(night_flat$tagname))+.001

low_night_sb_plot <- ggplot(data = night_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Night of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Night for Low Activity Birds (n=10)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/night_daily_time_budget_stack_bar_for_low_act",".png"), low_night_sb_plot,width = 5, height = 3, units = "in")

### End Low Activity Nightly Time Budget Plot ###

### Make Medium Activity Nightly Time Budget Plot ###

# Select the overall low activity bird ids

# n = 18
med_activity_ids <- overall_med_act$tagname

# read in the overall time budgets from low activity birds

med_night_tb <- Sys.glob(paste0("../intermediate/all_rooms/*night_*",med_activity_ids,".csv"))
med_night_df <- read_csv(med_night_tb)

night_flat <- cbind(med_night_df[c(1:2,6)], stack(med_night_df[3:5]))

night_flat$ind <- factor(night_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="1 days")

y_lim <- length(unique(night_flat$tagname))+.001

med_night_sb_plot <- ggplot(data = night_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Night of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Night for Medium Activity Birds (n=18)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/night_daily_time_budget_stack_bar_for_med_act",".png"), med_night_sb_plot,width = 5, height = 3, units = "in")

### End Medium Activity Nightly Time Budget Plot ###

### Make High Activity Nightly Time Budget Plot ###

# Select the overall low activity bird ids

# n = 9
high_activity_ids <- overall_high_act$tagname

# read in the overall time budgets from low activity birds

high_night_tb <- Sys.glob(paste0("../intermediate/all_rooms/*night_*",high_activity_ids,".csv"))
high_night_df <- read_csv(high_night_tb)

night_flat <- cbind(high_night_df[c(1:2,6)], stack(high_night_df[3:5]))

night_flat$ind <- factor(night_flat$ind, levels=c("Top","Middle","Bottom"))

datebreaks <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="7 days")

datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))))

all_datebreak <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="1 days")

y_lim <- length(unique(night_flat$tagname))+.001

high_night_sb_plot <- ggplot(data = night_flat, aes(x = as.Date(interval1), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw() +  
xlab("Night of Study") + 
ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
ggtitle("Daily Time Budget for Each Night for High Activity Birds (n=9)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/night_daily_time_budget_stack_bar_for_high_act",".png"), high_night_sb_plot,width = 5, height = 3, units = "in")

### End High Activity Nightly Time Budget Plot ###

### K-Means Clustering ### 

# Separate room ID n-trans

rm_2 <- data.frame(rm_2_org_overall[,2])
rownames(rm_2) <- rm_2_org_overall$tagname


png("../figures/all_day/rm_2_density.png")
plot(density(rm_2[,1]))
dev.off()

png("../figures/rm_2_elbow.png")
k.max <- 10 # Maximal number of clusters
data <- rm_2
wss <- sapply(1:k.max, 
        function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)
dev.off()

set.seed(123)
km.5 <- kmeans(rm_2, 5)
km.5

rm_2$cluster_5_id <- factor(km.5$cluster)

set.seed(123)
km.3 <- kmeans(rm_2, 3)
km.3

rm_2$cluster_3_id <- factor(km.3$cluster)

write.csv("../intermediate/rm_2_k_cluster.csv",row.name=T)

### END K-Means Clustering ###