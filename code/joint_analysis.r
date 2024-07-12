#### All Room: 2,3,8,11 Joint analysis ####

#source("./rfid_functions.R")

# Generate Transition tables from Room 11
library(xts)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tsibble)
library(testthat)
library(cluster)
library(purrr)
library(LTS)

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

(l2m <- summary_overall[2])

(m2h <- summary_overall[5])

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
high_act_day_tb <- nest_day_tb[nest_day_tb$tagname %in% overall_high$tagname,]


high_act_nest <- high_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)


high_act_nest_day <- high_act_day_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))

# select med activity birds
overall_med <- overall_org_table[overall_org_table$activity == "medium",]

# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_med$tagname,]
med_act_day_tb <- nest_day_tb[nest_day_tb$tagname %in% overall_med$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)

med_act_nest_day <- med_act_day_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))

# select low activity birds
overall_low <- overall_org_table[overall_org_table$activity == "low",]

# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_low$tagname,]
low_act_day_tb <- nest_day_tb[nest_day_tb$tagname %in% overall_low$tagname,]

low_act_nest_day <- low_act_day_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
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

low_tab_day <- colSums(table(low_act_nest_day[,c(1,3)])[,c("Bottom","Middle","Top")])
med_tab_day <- colSums(table(med_act_nest_day[,c(1,3)])[,c("Bottom","Middle","Top")])
high_tab_day <- colSums(table(high_act_nest_day[,c(1,3)])[,c("Bottom","Middle","Top")])

low_tot_day <- sum(low_tab_day)
med_tot_day <- sum(med_tab_day)
high_tot_day <- sum(high_tab_day)
tot_tot_day <- sum(low_tot_day,med_tot_day,high_tot_day)

absolute_nest_day <- data.frame(rbind(low_tab_day, med_tab_day, high_tab_day))
rownames(absolute_nest_day) <- c("low","medium","high")
absolute_nest_day$n <- c(low_tot_day,med_tot_day,high_tot_day)

write.csv(absolute_nest_day, "../intermediate/absolute_nest_day.csv")

relative_nest_day <- data.frame(rbind(low_tab_day/low_tot_day, med_tab_day/med_tot_day, high_tab_day/high_tot_day))
rownames(relative_nest_day) <- c("low","medium","high")
relative_nest_day$n <- c(low_tot_day,med_tot_day,high_tot_day)

write.csv(relative_nest_day, "../intermediate/relative_nest_day.csv")

### END Activity classification with overall table ###

### Find prop of time spent in zones over whole study ###


high_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_high$tagname,]
high_act_day_tb <- nest_day_tb[nest_day_tb$tagname %in% overall_high$tagname,]


high_act_tb_night <- high_act_night_tb |> 
  mutate(botProp = map(data, ~colMeans(na.omit(.x[,3])))) |>
  mutate(midProp = map(data, ~colMeans(na.omit(.x[,4])))) |>
  mutate(topProp = map(data, ~colMeans(na.omit(.x[,5])))) |>
  unnest(c(botProp,midProp,topProp))


high_act_tb_day <- high_act_day_tb |> 
  mutate(botProp = map(data, ~colMeans(na.omit(.x[,3])))) |>
  mutate(midProp = map(data, ~colMeans(na.omit(.x[,4])))) |>
  mutate(topProp = map(data, ~colMeans(na.omit(.x[,5])))) |>
  unnest(c(botProp,midProp,topProp))

# select med activity birds
overall_med <- overall_org_table[overall_org_table$activity == "medium",]

# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_med$tagname,]
med_act_day_tb <- nest_day_tb[nest_day_tb$tagname %in% overall_med$tagname,]

med_act_tb_night <- med_act_night_tb |> 
  mutate(botProp = map(data, ~colMeans(na.omit(.x[,3])))) |>
  mutate(midProp = map(data, ~colMeans(na.omit(.x[,4])))) |>
  mutate(topProp = map(data, ~colMeans(na.omit(.x[,5])))) |>
  unnest(c(botProp,midProp,topProp))


med_act_tb_day <- med_act_day_tb |> 
  mutate(botProp = map(data, ~colMeans(na.omit(.x[,3])))) |>
  mutate(midProp = map(data, ~colMeans(na.omit(.x[,4])))) |>
  mutate(topProp = map(data, ~colMeans(na.omit(.x[,5])))) |>
  unnest(c(botProp,midProp,topProp))



print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))

# select low activity birds
overall_low <- overall_org_table[overall_org_table$activity == "low",]

# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% overall_low$tagname,]
low_act_day_tb <- nest_day_tb[nest_day_tb$tagname %in% overall_low$tagname,]

low_act_tb_day <- low_act_day_tb |> 
  mutate(botProp = map(data, ~colMeans(na.omit(.x[,3])))) |>
  mutate(midProp = map(data, ~colMeans(na.omit(.x[,4])))) |>
  mutate(topProp = map(data, ~colMeans(na.omit(.x[,5])))) |>
  unnest(c(botProp,midProp,topProp))


low_act_tb_night <- low_act_night_tb |> 
  mutate(botProp = map(data, ~colMeans(na.omit(.x[,3])))) |>
  mutate(midProp = map(data, ~colMeans(na.omit(.x[,4])))) |>
  mutate(topProp = map(data, ~colMeans(na.omit(.x[,5])))) |>
  unnest(c(botProp,midProp,topProp))

low_night <- colMeans(low_act_tb_night[,3:5])
med_night <- colMeans(med_act_tb_night[,3:5])
high_night <- colMeans(high_act_tb_night[,3:5])

(low_day <- colMeans(low_act_tb_day[,3:5]))
(med_day <- colMeans(med_act_tb_day[,3:5]))
(high_day <- colMeans(high_act_tb_day[,3:5]))

day_tab <- round(rbind(low_day,med_day,high_day)*100,2)
write.csv(day_tab, "../output/day_table_prop_time.csv", row.names=T)


### END find prop time spent in zone over whole study ###


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
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_2_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_2_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
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
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_3_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_3_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
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
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_8_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_8_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
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
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)

print("Where do the high activity birds nest at night: ")
(sort(table(high_act_nest$nest),decreasing=T))


# select med time budgets based on ids

med_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_11_med_act$tagname,]

med_act_nest <- med_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
  unnest(nest)


print("Where do the medium activity birds nest at night: ")
(sort(table(med_act_nest$nest),decreasing=T))


# select low time budgets based on ids

low_act_night_tb <- nest_night_tb[nest_night_tb$tagname %in% rm_11_low_act$tagname,]

low_act_nest <- low_act_night_tb |> 
  mutate(nest = map(data, ~night_zone_from_TB(.x))) |>
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

day_tb_df$week <- week(as.POSIXct(day_tb_df$interval1,origin="1970-01-01"))

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
#png("../figures/all_day/model_diag/bottom_interaction_act_week.png")
bottom_act_week <- emmip(m1.bottom.means$jointMeans, activity~weekFac) + 
theme_bw(base_size=24, base_family='Times New Roman') + 
#xlab("Age (Weeks)") + ylab("Predicted Proportion of Time in Zone") + 
scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
scale_color_manual("Activity Class",
  values=c("low" ="#F8766D","medium"="#00BA38","high"="#619CFF"),
  breaks=c("high","medium","low"),
  labels=c("Hi","Me","Lo")) +
labs(title="Estimated Prop. Time Spent in Bottom Zone by Activity Class")
ggsave("../figures/all_day/model_diag/bottom_interaction_act_week.png",bottom_act_week, width = 8, height = 5, units = "in",dpi=300)

#dev.off()

# interaction plot of activity on x
#png("../figures/all_day/model_diag/bottom_interaction_week_act.png")
bottom_week_act <- emmip(m1.bottom.means$jointMeans, weekFac~activity)+ 
theme_bw() + 
xlab("Activity Class") + ylab("Predicted Proportion of Time in Zone") + 
scale_color_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42"))+
scale_x_discrete(labels=c("Lo","Me","Hi")) + 
labs(title="Estimated Prop. Time Spent in Bottom Zone by Week",color="Age (Weeks)")
ggsave("../figures/all_day/model_diag/bottom_interaction_week_act.png",bottom_week_act, width = 8, height = 5, units = "in")
#dev.off()

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

middle_act_week <- emmip(m2.middle.means$jointMeans, activity~weekFac) + 
theme_bw(base_size=24, base_family='Times New Roman') + 
#xlab("Age (Weeks)") + ylab("Predicted Proportion of Time in Zone") + 
scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
scale_color_manual("Activity Class",
  values=c("low" ="#F8766D","medium"="#00BA38","high"="#619CFF"),
  breaks=c("high","medium","low"),
  labels=c("Hi","Me","Lo")) +
labs(title="Estimated Prop. Time Spent in Middle Zone by Activity Class")
ggsave("../figures/all_day/model_diag/middle_interaction_act_week.png",middle_act_week, width = 8, height = 5, units = "in",dpi=300)

# interaction plot of activity on x
middle_week_act <- emmip(m2.middle.means$jointMeans, weekFac~activity) +
theme_bw(base_size=24, base_family='Times New Roman') + 
#xlab("Activity Class") + ylab("Predicted Proportion of Time in Zone") + 
scale_color_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42"))+
scale_x_discrete(labels=c("Lo","Me","Hi")) + 
labs(title="Estimated Prop. Time Spent in Middle Zone by Week",color="Age (Weeks)")
ggsave("../figures/all_day/model_diag/middle_interaction_week_act.png",middle_week_act, width = 8, height = 5, units = "in",dpi=300)



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

top_act_week <- emmip(m3.top.means$jointMeans, activity~weekFac)+ 
theme_bw(base_size=24, base_family='Times New Roman') + 
#xlab("Age (Weeks)") + ylab("Predicted Proportion of Time in Zone") + 
scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
scale_color_manual("Activity Class",
  values=c("low" ="#F8766D","medium"="#00BA38","high"="#619CFF"),
  breaks=c("high","medium","low"),
  labels=c("Hi","Me","Lo")) +
labs(title="Estimated Prop. Time Spent in Top Zone by Activity Class")
ggsave("../figures/all_day/model_diag/top_interaction_act_week.png",top_act_week, width = 8, height = 5, units = "in",dpi=300)


# interaction plot of activity on x
png("../figures/all_day/model_diag/top_interaction_week_act.png")
top_week_act <- emmip(m3.top.means$jointMeans, weekFac~activity) +
theme_bw(base_size=24, base_family='Times New Roman') + 
#xlab("Activity Class") + ylab("Predicted Proportion of Time in Zone") + 
scale_color_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42"))+
scale_x_discrete(labels=c("Lo","Me","Hi")) + 
labs(title="Estimated Prop. Time Spent in Top Zone by Week",color="Age (Weeks)")
ggsave("../figures/all_day/model_diag/top_interaction_week_act.png",top_week_act, width = 8, height = 5, units = "in",dpi=300)


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

keel_score <- read_table('../data/keel_score/keel-scores-2-14-2024.tsv')

# join keel score to overall day summary table

unique(overall_day_summary$tagname) %in% keel_score$tag


overall_day_summary_w_keel <- data.frame(merge(overall_day_summary,keel_score, by.x="tagname",by.y="tag"))

overall_day_summary_w_keel$keel_score <- as.numeric(overall_day_summary_w_keel$keelScore)

print("Tagnames Obeserved with Keel Score: ")
print(unique(na.exclude(overall_day_summary_w_keel[,c(1,9)])))
print(length(unique(na.exclude(overall_day_summary_w_keel[,c(1,9)]))[,1]))


raw_n_keel <-  data.frame(table(unique(overall_day_summary_w_keel[,c(1,6,9)])))
(sum_n_keel <- raw_n_keel %>% group_by(activity,keelScore) %>%  summarise(n = sum(Freq)))

write.csv(sum_n_keel, "../intermediate/n_keel.csv", row.names=F)

# TODO update these to be better named
tmp2 <- unique(overall_day_summary_w_keel %>% select(c(tagname,activity,rm,keelScore)))
tmp2$tagname <- factor(tmp2$tagname,levels=tmp2$tagname)
tmp2$tag <- as.numeric(tmp2$tagname)



m4 <- lmer(keelScore ~ activity + (1|rm), tmp2)
summary(m4)
anova(m4)

# # TODO implement kruskal
# kruskal.test()

m4_means <- emmeans(m4, specs=list( actMean = ~activity))

c1 <- c(-1,.5,.5)
c2 <- c(0,-1,1)

contrast(m4_means, method=list(low.vs.medHigh = c1,
med.vs.high = c2),adjust="bonferroni")

png("../figures/all_day/model_diag/keel_score_mean_m4.png",width=5,height=3, units='in',res=300)
plot(m4_means, horizontal = F,comparisons=T)
dev.off()

kruskal.test(keel_score ~ activity, overall_day_summary_w_keel)

m5 <- aov(keel_score ~ activity, overall_day_summary_w_keel)
summary(m5)
m5.res <- resid(m5)

png("../figures/all_day/model_diag/keel_score_qq.png")
qqnorm(m5.res)
qqline(m5.res)
dev.off()

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

datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(day_flat$interval1),n=1),origin="1970-01-01")),
as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(day_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")), by="1 days")

y_lim <- length(unique(day_flat$tagname))+.001

low_day_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(as.POSIXct.numeric(as.numeric(interval1),origin="1970-01-01")), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw(base_size=24, base_family='Times New Roman') +  
#xlab("Day of Study") + 
#ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#ggtitle("Daily Time Budget for Each Day for Low Activity Birds (n=10)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))

#TODO send regmi this figure
ggsave(paste0("../figures/all_day/all_rooms/day_daily_time_budget_stack_bar_for_low_act",".png"), low_day_sb_plot,width = 5, height = 3, units = "in",dpi=300)

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

datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(day_flat$interval1),n=1),origin="1970-01-01")),
as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(day_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")), by="1 days")

y_lim <- length(unique(day_flat$tagname))+.001

med_day_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(as.POSIXct.numeric(as.numeric(interval1),origin="1970-01-01")), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw(base_size=24, base_family='Times New Roman') +  
#xlab("Day of Study") + 
#ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#ggtitle("Daily Time Budget for Each Day for Medium Activity Birds (n=18)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/day_daily_time_budget_stack_bar_for_med_act",".png"), med_day_sb_plot,width = 5, height = 3, units = "in",dpi=300)

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

datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(day_flat$interval1),n=1),origin="1970-01-01")),
as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(day_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(day_flat$interval1),n=1),origin="1970-01-01")), by="1 days")

y_lim <- length(unique(day_flat$tagname))+.001

high_day_sb_plot <- ggplot(data = day_flat, aes(x = as.Date(as.POSIXct.numeric(as.numeric(interval1),origin="1970-01-01")), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw(base_size=24, base_family='Times New Roman') +  
#xlab("Day of Study") + 
#ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#ggtitle("Daily Time Budget for Each Day for High Activity Birds (n=9)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/day_daily_time_budget_stack_bar_for_high_act",".png"), high_day_sb_plot,width = 5, height = 3, units = "in",dpi=300)

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

datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(night_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(night_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")), by="1 days")

y_lim <- length(unique(night_flat$tagname))+.001

low_night_sb_plot <- ggplot(data = night_flat, aes(x = as.Date(as.POSIXct.numeric(as.numeric(interval1),origin="1970-01-01")), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw(base_size=24, base_family='Times New Roman') +  
#xlab("Night of Study") + 
#ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#ggtitle("Daily Time Budget for Each Night for Low Activity Birds (n=10)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/night_daily_time_budget_stack_bar_for_low_act",".png"), low_night_sb_plot,width = 5, height = 3, units = "in",dpi=300)

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

# datebreaks <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="7 days")

# datebreaks <- c(datebreaks, as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))))

# all_datebreak <- seq(as.Date(ymd_hms(head(unique(night_flat$interval1),n=1))), as.Date(ymd_hms(tail(unique(night_flat$interval1),n=1))), by="1 days")


datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(night_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(night_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")), by="1 days")

y_lim <- length(unique(night_flat$tagname))+.001

med_night_sb_plot <- ggplot(data = night_flat, aes(x = as.Date(as.POSIXct.numeric(as.numeric(interval1),origin="1970-01-01")), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw(base_size=24, base_family='Times New Roman') +  
#xlab("Night of Study") + 
#ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#ggtitle("Daily Time Budget for Each Night for Medium Activity Birds (n=18)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/night_daily_time_budget_stack_bar_for_med_act",".png"), med_night_sb_plot,width = 5, height = 3, units = "in",dpi=300)

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

datebreaks <- seq(as.Date(as.POSIXct.numeric(head(unique(night_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")), by="7 days")

datebreaks <- c(datebreaks, as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")))

all_datebreak <- seq(as.Date(as.POSIXct.numeric(head(unique(night_flat$interval1),n=1),origin="1970-01-01")), as.Date(as.POSIXct.numeric(tail(unique(night_flat$interval1),n=1),origin="1970-01-01")), by="1 days")

y_lim <- length(unique(night_flat$tagname))+.001

high_night_sb_plot <- ggplot(data = night_flat, aes(x = as.Date(as.POSIXct.numeric(as.numeric(interval1),origin="1970-01-01")), y=values, fill=ind)) + 
geom_bar(stat="identity") +
theme_bw(base_size=24, base_family='Times New Roman') +  
#xlab("Night of Study") + 
#ylab("Prop. of Time Spent in Zone") +
scale_x_date(breaks= datebreaks, minor_breaks=all_datebreak) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#ggtitle("Daily Time Budget for Each Night for High Activity Birds (n=9)") + 
labs(fill = "Zone") +
scale_y_continuous(limits=c(0, y_lim))


ggsave(paste0("../figures/all_day/all_rooms/night_daily_time_budget_stack_bar_for_high_act",".png"), high_night_sb_plot,width = 5, height = 3, units = "in",dpi=300)

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

write.csv(rm_2,"../intermediate/rm_2_k_cluster.csv",row.name=T)

### END K-Means Clustering ###

### Make Weekly Transitions of Most Active Bird ###

# identify most active bird
overall_table[order(overall_table$ntrans, decreasing=T),]

#6905 | 3

high_bird_1 <- rm_3_day_int_df[rm_3_day_int_df$tagname == "6905",]
high_bird_1$w_start <- week(as.POSIXct(high_bird_1$t1,origin="1970-01-01"))

# turn into weeks, and then calculate ntrans per week

high_bird_1_sum <- high_bird_1 |> nest(data = -w_start) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

high_bird_1_long <- pivot_longer(high_bird_1_sum,col=c('nbot','nmid','ntop'))

high_bird_1_long$w_start <- as.factor(high_bird_1_long$w_start)
high_bird_1_long$name <- as.factor(high_bird_1_long$name)

hb_1 <- ggplot(data=high_bird_1_long, aes(x=w_start, y=value, group=name)) +
  theme_bw(base_size=24, base_family='Times New Roman')+
  geom_line()+
  geom_point() + 
  #xlab("Age (Weeks)") + ylab("Number of transitions into zone") + 
  scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))#+
  #labs(title="Number of Transitions Most Active (6905, n=1)")


ggsave(paste0("../figures/all_day/transition_plots/6905_most_active_trans_per_week",".png"), hb_1,width = 5, height = 3, units = "in",dpi=300)


#6998 | 3
high_bird_2 <- rm_3_day_int_df[rm_3_day_int_df$tagname == "6998",]
high_bird_2$w_start <- week(as.POSIXct(high_bird_2$t1,origin="1970-01-01"))

# turn into weeks, and then calculate ntrans per week

high_bird_2_sum <- high_bird_2 |> nest(data = -w_start) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

high_bird_2_long <- pivot_longer(high_bird_2_sum,col=c('nbot','nmid','ntop'))

high_bird_2_long$w_start <- as.factor(high_bird_2_long$w_start)
high_bird_2_long$name <- as.factor(high_bird_2_long$name)

hb_2 <- ggplot(data=high_bird_2_long, aes(x=w_start, y=value, group=name)) +
  geom_line()+
  geom_point() + 
  theme_bw(base_size=24, base_family='Times New Roman')+
  #xlab("Age (Weeks)") + ylab("Number of transitions into zone") + 
  scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))#+
  #labs(title="Number of Transitions Most Active (6998, n=1)")


ggsave(paste0("../figures/all_day/transition_plots/6998_most_active_trans_per_week",".png"), hb_2,width = 5, height = 3, units = "in",dpi=300)



### END Make Weekly Transitions of Most Active Bird ###

### Make Weekly Transitions of Least Active Bird ###\

# identify least active bird

overall_table[order(overall_table$ntrans, decreasing=F),]
#6929 | 8 

low_bird <- rm_8_day_int_df[rm_8_day_int_df$tagname == "6929",]
low_bird$w_start <- week(as.POSIXct(low_bird$t1,origin="1970-01-01"))

# turn into weeks, and then calculate ntrans per week

low_bird_sum <- low_bird |> nest(data = -w_start) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

low_bird_long <- pivot_longer(low_bird_sum,col=c('nbot','nmid','ntop'))

low_bird_long$w_start <- as.factor(low_bird_long$w_start)
low_bird_long$name <- as.factor(low_bird_long$name)

lb <- ggplot(data=low_bird_long, aes(x=w_start, y=value, group=name)) +
  geom_line()+
  geom_point() +
  theme_bw(base_size=24, base_family='Times New Roman') +
  #xlab("Age (Weeks)") + ylab("Number of transitions into zone") + 
  scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))#+
  #labs(title="Number of Transitions Least Active (6929, n=1)")


ggsave(paste0("../figures/all_day/transition_plots/6929_least_active_trans_per_week",".png"), lb,width = 5, height = 3, units = "in",dpi=300)

### END Make Weekly Transitions of Least Active Bird ###

### Make Weekly Transitions of Low Activity Birds ###

low_bird_ids <- overall_org_table[overall_org_table$activity == "low","tagname"][[1]]

# get low act int from room 2

low_act_bird <- rm_2_day_int_df[rm_2_day_int_df$tagname %in% low_bird_ids,]

# get low act int from room 3
low_act_bird <- rbind(low_act_bird, rm_3_day_int_df[rm_3_day_int_df$tagname %in% low_bird_ids,])

# get low act int from room 8
low_act_bird <- rbind(low_act_bird, rm_8_day_int_df[rm_8_day_int_df$tagname %in% low_bird_ids,])

# get low act int from room 11
low_act_bird <- rbind(low_act_bird, rm_11_day_int_df[rm_11_day_int_df$tagname %in% low_bird_ids,])

# turn into weekly tables and calc num trans
low_act_bird$w_start <- week(as.POSIXct(low_act_bird$t1,origin="1970-01-01"))


low_act_bird_sum <- low_act_bird |> nest(data = -w_start) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

low_act_bird_long <- pivot_longer(low_act_bird_sum,col=c('nbot','nmid','ntop'))

low_act_bird_long$w_start <- as.factor(low_act_bird_long$w_start)
low_act_bird_long$name <- as.factor(low_act_bird_long$name)

lab <- ggplot(data=low_act_bird_long, aes(x=w_start, y=value, group=name)) +
  geom_line()+
  geom_point() +
  theme+bw(base_size=24, base_family='Times New Roman')+ 
  #xlab("Age (Weeks)") + ylab("Number of transitions into zone") + 
  scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  #labs(title="Number of Transitions Least Active Birds (n=32)") +
  ylim(0,4200)


ggsave(paste0("../figures/all_day/transition_plots/least_active_birds_trans_per_week",".png"), lab,width = 5, height = 3, units = "in",dpi=300)


### END Make Weekly Transitions of Low Activity Birds ###

### Make Weekly Transitions of  Medium Activity Birds ###

med_bird_ids <- overall_org_table[overall_org_table$activity == "medium","tagname"][[1]]

# get med act int from room 2

med_act_bird <- rm_2_day_int_df[rm_2_day_int_df$tagname %in% med_bird_ids,]

# get med act int from room 3
med_act_bird <- rbind(med_act_bird, rm_3_day_int_df[rm_3_day_int_df$tagname %in% med_bird_ids,])

# get med act int from room 8
med_act_bird <- rbind(med_act_bird, rm_8_day_int_df[rm_8_day_int_df$tagname %in% med_bird_ids,])

# get med act int from room 11
med_act_bird <- rbind(med_act_bird, rm_11_day_int_df[rm_11_day_int_df$tagname %in% med_bird_ids,])

# turn into weekly tables and calc num trans
med_act_bird$w_start <- week(as.POSIXct(med_act_bird$t1,origin="1970-01-01"))


med_act_bird_sum <- med_act_bird |> nest(data = -w_start) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

med_act_bird_long <- pivot_longer(med_act_bird_sum,col=c('nbot','nmid','ntop'))

med_act_bird_long$w_start <- as.factor(med_act_bird_long$w_start)
med_act_bird_long$name <- as.factor(med_act_bird_long$name)

mab <- ggplot(data=med_act_bird_long, aes(x=w_start, y=value, group=name)) +
  geom_line()+
  geom_point() + 
  theme_bw(base_size=24, base_family='Times New Roman')+
  #xlab("Age (Weeks)") + ylab("Number of transitions into zone") + 
  scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  #labs(title="Number of Transitions Medium Active Birds (n=62)")+
  ylim(0,4200)


ggsave(paste0("../figures/all_day/transition_plots/medium_active_birds_trans_per_week",".png"), mab,width = 5, height = 3, units = "in",dpi=300)



### END Make Weekly Transitions of Medium Activity Birds ###

### Make Weekly Transitions of High Activity Birds ###
high_bird_ids <- overall_org_table[overall_org_table$activity == "high","tagname"][[1]]

# get high act int from room 2

high_act_bird <- rm_2_day_int_df[rm_2_day_int_df$tagname %in% high_bird_ids,]

# get high act int from room 3
high_act_bird <- rbind(high_act_bird, rm_3_day_int_df[rm_3_day_int_df$tagname %in% high_bird_ids,])

# get high act int from room 8
high_act_bird <- rbind(high_act_bird, rm_8_day_int_df[rm_8_day_int_df$tagname %in% high_bird_ids,])

# get high act int from room 11
high_act_bird <- rbind(high_act_bird, rm_11_day_int_df[rm_11_day_int_df$tagname %in% high_bird_ids,])

# turn into weekly tables and calc num trans
high_act_bird$w_start <- week(as.POSIXct(high_act_bird$t1,origin="1970-01-01"))


high_act_bird_sum <- high_act_bird |> nest(data = -w_start) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

high_act_bird_long <- pivot_longer(high_act_bird_sum,col=c('nbot','nmid','ntop'))

high_act_bird_long$w_start <- as.factor(high_act_bird_long$w_start)
high_act_bird_long$name <- as.factor(high_act_bird_long$name)

hab <- ggplot(data=high_act_bird_long, aes(x=w_start, y=value, group=name)) +
  geom_line()+
  geom_point() + 
  theme_bw(base_size=24, base_family='Times New Roman')+
  #xlab("Age (Weeks)") + ylab("Number of transitions into zone") + 
  scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  #labs(title="Number of Transitions Most Active Birds (n=31)")+
  ylim(0,4200)


ggsave(paste0("../figures/all_day/transition_plots/most_active_birds_trans_per_week",".png"), hab,width = 5, height = 3, units = "in",dpi=300)



### END Make Weekly Transitions of High Activity Birds ###

### Make Ave. Weekly Transitions ###

low_bird_ids <- overall_org_table[overall_org_table$activity == "low","tagname"][[1]]

# get low act int from room 2

low_act_bird <- rm_2_day_int_df[rm_2_day_int_df$tagname %in% low_bird_ids,]

# get low act int from room 3
low_act_bird <- rbind(low_act_bird, rm_3_day_int_df[rm_3_day_int_df$tagname %in% low_bird_ids,])

# get low act int from room 8
low_act_bird <- rbind(low_act_bird, rm_8_day_int_df[rm_8_day_int_df$tagname %in% low_bird_ids,])

# get low act int from room 11
low_act_bird <- rbind(low_act_bird, rm_11_day_int_df[rm_11_day_int_df$tagname %in% low_bird_ids,])

# turn into weekly tables and calc num trans
low_act_bird$w_start <- week(as.POSIXct(low_act_bird$t1,origin="1970-01-01"))


low_act_bird_sum <- low_act_bird |> nest(data = -w_start) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

low_act_bird_ave <- round(low_act_bird_sum[,c(2:4)]/length(low_bird_ids),2)
low_act_bird_ave$w_start <- low_act_bird_sum$w_start

low_act_bird_long <- pivot_longer(low_act_bird_ave,col=c('nbot','nmid','ntop'))

low_act_bird_long$w_start <- as.factor(low_act_bird_long$w_start)
low_act_bird_long$name <- as.factor(low_act_bird_long$name)

lab <- ggplot(data=low_act_bird_long, aes(x=w_start, y=value, group=name)) +
  geom_line()+
  geom_point() + 
  theme_bw(base_size=24, base_family='Times New Roman')+
  #xlab("Age (Weeks)") + ylab("Number of Average transitions into zone") + 
  scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  #labs(title=paste("Number of Transitions Least Active Birds (n=",length(low_bird_ids) ,")")) +
  ylim(0,135)


ggsave(paste0("../figures/all_day/transition_plots/least_active_birds_average_trans_per_week",".png"), lab,width = 5, height = 3, units = "in",dpi=300)


### END Make Weekly Transitions of Low Activity Birds ###

### Make Weekly Transitions of  Medium Activity Birds ###

med_bird_ids <- overall_org_table[overall_org_table$activity == "medium","tagname"][[1]]

# get med act int from room 2

med_act_bird <- rm_2_day_int_df[rm_2_day_int_df$tagname %in% med_bird_ids,]

# get med act int from room 3
med_act_bird <- rbind(med_act_bird, rm_3_day_int_df[rm_3_day_int_df$tagname %in% med_bird_ids,])

# get med act int from room 8
med_act_bird <- rbind(med_act_bird, rm_8_day_int_df[rm_8_day_int_df$tagname %in% med_bird_ids,])

# get med act int from room 11
med_act_bird <- rbind(med_act_bird, rm_11_day_int_df[rm_11_day_int_df$tagname %in% med_bird_ids,])

# turn into weekly tables and calc num trans
med_act_bird$w_start <- week(as.POSIXct(med_act_bird$t1,origin="1970-01-01"))


med_act_bird_sum <- med_act_bird |> nest(data = -w_start) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))


med_act_bird_ave <- round(med_act_bird_sum[,c(2:4)]/length(med_bird_ids),2)
med_act_bird_ave$w_start <- med_act_bird_sum$w_start


med_act_bird_long <- pivot_longer(med_act_bird_ave,col=c('nbot','nmid','ntop'))

med_act_bird_long$w_start <- as.factor(med_act_bird_long$w_start)
med_act_bird_long$name <- as.factor(med_act_bird_long$name)

mab <- ggplot(data=med_act_bird_long, aes(x=w_start, y=value, group=name)) +
  geom_line()+
  geom_point() +
  theme_bw(base_size=24, base_family='Times New Roman')+ 
  #xlab("Age (Weeks)") + ylab("Average Number of transitions into zone") + 
  scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  #labs(title=paste("Number of Transitions Medium Active Birds (n=",length(med_bird_ids),")"))+
  ylim(0,135)


ggsave(paste0("../figures/all_day/transition_plots/medium_active_birds_average_trans_per_week",".png"), mab,width = 5, height = 3, units = "in",dpi=300)



### END Make Weekly Transitions of Medium Activity Birds ###

### Make Weekly Transitions of High Activity Birds ###
high_bird_ids <- overall_org_table[overall_org_table$activity == "high","tagname"][[1]]

# get high act int from room 2

high_act_bird <- rm_2_day_int_df[rm_2_day_int_df$tagname %in% high_bird_ids,]

# get high act int from room 3
high_act_bird <- rbind(high_act_bird, rm_3_day_int_df[rm_3_day_int_df$tagname %in% high_bird_ids,])

# get high act int from room 8
high_act_bird <- rbind(high_act_bird, rm_8_day_int_df[rm_8_day_int_df$tagname %in% high_bird_ids,])

# get high act int from room 11
high_act_bird <- rbind(high_act_bird, rm_11_day_int_df[rm_11_day_int_df$tagname %in% high_bird_ids,])

# turn into weekly tables and calc num trans
high_act_bird$w_start <- week(as.POSIXct(high_act_bird$t1,origin="1970-01-01"))


high_act_bird_sum <- high_act_bird |> nest(data = -w_start) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

high_act_bird_ave <- round(high_act_bird_sum[,c(2:4)]/length(high_bird_ids),2)
high_act_bird_ave$w_start <- high_act_bird_sum$w_start

high_act_bird_long <- pivot_longer(high_act_bird_ave,col=c('nbot','nmid','ntop'))

high_act_bird_long$w_start <- as.factor(high_act_bird_long$w_start)
high_act_bird_long$name <- as.factor(high_act_bird_long$name)

hab <- ggplot(data=high_act_bird_long, aes(x=w_start, y=value, group=name)) +
  geom_line()+
  geom_point() + 
  theme_bw(base_size=24, base_family='Times New Roman')+
  #xlab("Age (Weeks)") + ylab("Average Number of transitions into zone") + 
  scale_x_discrete(labels=c("10" = "34", 
                          "11" = "35",
                          "12" = "36",
                          "13" = "37",
                          "14" = "38",
                          "15" = "39",
                          "16" = "40",
                          "17" = "41",
                          "18" = "42")) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  #labs(title=paste("Number of Transitions Most Active Birds (n=",length(high_bird_ids),")"))+
  ylim(0,135)


ggsave(paste0("../figures/all_day/transition_plots/most_active_birds_average_trans_per_week",".png"), hab,width = 5, height = 3, units = "in",dpi=300)

### END Make ave. Weekly Transitions of High Activity Birds ###

### Make Ave. Daily Transitions ###

low_bird_ids <- overall_org_table[overall_org_table$activity == "low","tagname"][[1]]

# get low act int from room 2

low_act_bird <- rm_2_day_int_df[rm_2_day_int_df$tagname %in% low_bird_ids,]

# get low act int from room 3
low_act_bird <- rbind(low_act_bird, rm_3_day_int_df[rm_3_day_int_df$tagname %in% low_bird_ids,])

# get low act int from room 8
low_act_bird <- rbind(low_act_bird, rm_8_day_int_df[rm_8_day_int_df$tagname %in% low_bird_ids,])

# get low act int from room 11 
low_act_bird <- rbind(low_act_bird, rm_11_day_int_df[rm_11_day_int_df$tagname %in% low_bird_ids,])

# turn into daily tables and calc num trans
date_to_day <- data.frame(cbind(unique(date(as.POSIXct(low_act_bird$t1,origin="1970-01-01"))),1:length(unique(date(as.POSIXct(low_act_bird$t1,origin=origin))))))
colnames(date_to_day) <- c("date","day")
low_act_bird$w_start <- week(as.POSIXct(low_act_bird$t1,origin="1970-01-01"))
low_act_bird$date <- date(as.POSIXct(low_act_bird$t1,origin="1970-01-01"))
low_act_bird <- data.frame(merge(low_act_bird,date_to_day, by.x="date",by.y="date"))

overall_day_summary <- data.frame(merge(overall_day_summary,overall_org_table[,c(1,4)], by="tagname"))

low_act_bird_sum <- low_act_bird |> nest(data = -c(day,w_start)) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(day,w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

low_act_bird_ave <- round(low_act_bird_sum[,c(3:5)]/length(low_bird_ids),2)
low_act_bird_ave$d_start <- low_act_bird_sum$day
low_act_bird_ave$w_start <- low_act_bird_sum$w_start

low_act_bird_long <- pivot_longer(low_act_bird_ave,col=c('nbot','nmid','ntop'))

low_act_bird_long$d_start <- as.numeric(low_act_bird_long$d_start)
low_act_bird_long$w_start <- as.numeric(low_act_bird_long$w_start)
low_act_bird_long$name <- as.factor(low_act_bird_long$name)

low_act <- aggregate(low_act_bird_long$value, list(low_act_bird_long$name), FUN=mean) 
low_med_act <- aggregate(low_act_bird_long$value, list(low_act_bird_long$name), FUN=median) 

### END Make Daily Transitions of Low Activity Birds ###

### Make Daily Transitions of  Medium Activity Birds ###

med_bird_ids <- overall_org_table[overall_org_table$activity == "medium","tagname"][[1]]

# get med act int from room 2

med_act_bird <- rm_2_day_int_df[rm_2_day_int_df$tagname %in% med_bird_ids,]

# get med act int from room 3
med_act_bird <- rbind(med_act_bird, rm_3_day_int_df[rm_3_day_int_df$tagname %in% med_bird_ids,])

# get med act int from room 8
med_act_bird <- rbind(med_act_bird, rm_8_day_int_df[rm_8_day_int_df$tagname %in% med_bird_ids,])

# get med act int from room 11
med_act_bird <- rbind(med_act_bird, rm_11_day_int_df[rm_11_day_int_df$tagname %in% med_bird_ids,])

# turn into daily tables and calc num trans
med_act_bird$date <- date(as.POSIXct(med_act_bird$t1,origin="1970-01-01"))
med_act_bird$w_start <- week(as.POSIXct(med_act_bird$t1,origin="1970-01-01"))
med_act_bird <- data.frame(merge(med_act_bird,date_to_day, by.x="date",by.y="date"))


med_act_bird_sum <- med_act_bird |> nest(data = -c(day,w_start)) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(day,w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))


med_act_bird_ave <- round(med_act_bird_sum[,c(3:5)]/length(med_bird_ids),2)
med_act_bird_ave$d_start <- med_act_bird_sum$day
med_act_bird_ave$w_start <- med_act_bird_sum$w_start


med_act_bird_long <- pivot_longer(med_act_bird_ave,col=c('nbot','nmid','ntop'))

med_act_bird_long$d_start <- as.numeric(med_act_bird_long$d_start)
med_act_bird_long$w_start <- as.numeric(med_act_bird_long$w_start)
med_act_bird_long$name <- as.factor(med_act_bird_long$name)

med_act <- aggregate(med_act_bird_long$value, list(med_act_bird_long$name), FUN=mean) 
med_med_act <- aggregate(med_act_bird_long$value, list(med_act_bird_long$name), FUN=median)

### END Make Daily Transitions of Medium Activity Birds ###

### Make Daily Transitions of High Activity Birds ###
high_bird_ids <- overall_org_table[overall_org_table$activity == "high","tagname"][[1]]

# get high act int from room 2

high_act_bird <- rm_2_day_int_df[rm_2_day_int_df$tagname %in% high_bird_ids,]

# get high act int from room 3
high_act_bird <- rbind(high_act_bird, rm_3_day_int_df[rm_3_day_int_df$tagname %in% high_bird_ids,])

# get high act int from room 8
high_act_bird <- rbind(high_act_bird, rm_8_day_int_df[rm_8_day_int_df$tagname %in% high_bird_ids,])

# get high act int from room 11
high_act_bird <- rbind(high_act_bird, rm_11_day_int_df[rm_11_day_int_df$tagname %in% high_bird_ids,])

# turn into daily tables and calc num trans
high_act_bird$date <- date(as.POSIXct(high_act_bird$t1,origin="1970-01-01"))
high_act_bird$w_start <- week(as.POSIXct(high_act_bird$t1,origin="1970-01-01"))
high_act_bird <- data.frame(merge(high_act_bird,date_to_day, by.x="date",by.y="date"))

high_act_bird_sum <- high_act_bird |> nest(data = -c(day,w_start)) |>
  mutate(nbot = map(data, ~length(.x[.x$to_zone == "bottom",]$t1))) |>
  mutate(nmid = map(data, ~length(.x[.x$to_zone == "middle",]$t1))) |>
  mutate(ntop = map(data, ~length(.x[.x$to_zone == "top",]$t1))) |>
  select(c(day,w_start,nbot,nmid,ntop)) |>
  unnest(c(nbot,nmid,ntop))

high_act_bird_ave <- round(high_act_bird_sum[,c(3:5)]/length(high_bird_ids),2)
high_act_bird_ave$d_start <- high_act_bird_sum$day
high_act_bird_ave$w_start <- high_act_bird_sum$w_start

high_act_bird_long <- pivot_longer(high_act_bird_ave,col=c('nbot','nmid','ntop'))

high_act_bird_long$d_start <- as.numeric(high_act_bird_long$d_start)
high_act_bird_long$w_start <- as.numeric(high_act_bird_long$w_start)
high_act_bird_long$name <- as.factor(high_act_bird_long$name)

high_act <- aggregate(high_act_bird_long$value, list(high_act_bird_long$name), FUN=mean) 
high_med_act <- aggregate(high_act_bird_long$value, list(high_act_bird_long$name), FUN=median) 


### END Make ave. Daily Transitions of High Activity Birds ###

### Make Ave. Daily Transition Table ###

activity_trans_table <- cbind(low_act, med_act[,2], high_act[,2])
colnames(activity_trans_table) <- c("Zone","Low","Medium","High")

write.csv(activity_trans_table,"../output/daily_ave_transitions_by_activity.csv",row.names=F)

activity_med_trans_table <- cbind(low_med_act, med_med_act[,2], high_med_act[,2])
colnames(activity_med_trans_table) <- c("Zone","Low","Medium","High")

write.csv(activity_med_trans_table,"../output/daily_median_transitions_by_activity.csv",row.names=F)

### END make average trans tables ###

### Make Daily Transition Plots ###


low_breaks <- rbind(low_act_bird_long[1,],low_act_bird_long[which(low_act_bird_long$w_start != dplyr::lag(low_act_bird_long$w_start)),])


lab <- ggplot(data=low_act_bird_long, aes(x=d_start, y=value, group=name)) +
  geom_line()+
  geom_point() + 
  theme_bw(base_size=24, base_family='Times New Roman') + 
  #xlab("Age (Weeks)") + ylab("Number of Average transitions into zone") + 
  scale_x_continuous(breaks=low_breaks$d_start, 
                          labels=as.numeric(low_breaks$w_start)+24) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("",#"Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("",#"Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  #labs(title=paste("Ave Daily Transitions Least Active Birds (n=",length(low_bird_ids) ,")")) +
  ylim(0,25)


ggsave(paste0("../figures/all_day/transition_plots/least_active_birds_average_trans_per_day",".png"), lab, width = 8, height =5 , units = "in",dpi=300)


med_breaks <- rbind(med_act_bird_long[1,],med_act_bird_long[which(med_act_bird_long$w_start != dplyr::lag(med_act_bird_long$w_start)),])


mab <- ggplot(data=med_act_bird_long, aes(x=d_start, y=value, group=name)) +
  geom_line()+
  geom_point() + 
  theme_bw(base_size=24, base_family='Times New Roman') + 
  #xlab("Age (Weeks)") + ylab("Average Number of transitions into zone") + 
  scale_x_continuous(breaks=med_breaks$d_start, 
                          labels=as.numeric(med_breaks$w_start)+24) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("",#"Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("",#"Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  #labs(title=paste("Average Daily Transitions Medium Active Birds (n=",length(med_bird_ids),")"))+
  ylim(0,25)


ggsave(paste0("../figures/all_day/transition_plots/medium_active_birds_average_trans_per_day",".png"), mab, width = 8, height = 5, units = "in",dpi=300)


high_breaks <- rbind(high_act_bird_long[1,],high_act_bird_long[which(high_act_bird_long$w_start != dplyr::lag(high_act_bird_long$w_start)),])



hab <- ggplot(data=high_act_bird_long, aes(x=d_start, y=value, group=name)) +
  geom_line()+
  geom_point() +
  theme_bw(base_size=24, base_family='Times New Roman') +  
  #xlab("Age (Weeks)") + ylab("Average Number of transitions into zone") + 
  scale_x_continuous(breaks=high_breaks$d_start, 
                          labels=as.numeric(high_breaks$w_start)+24) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("",#"Transition into zone",
                      values=c("nbot" ="#F8766D","nmid"="#00BA38","ntop"="#619CFF"),
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  scale_fill_discrete("",#"Transition into zone",
                      breaks=c("ntop","nmid","nbot"), 
                      labels=c("Top","Middle","Bottom"))+
  #labs(title=paste("Average Daily Transitions Most Active Birds (n=",length(high_bird_ids),")"))+
  ylim(0,25)


ggsave(paste0("../figures/all_day/transition_plots/most_active_birds_average_trans_per_day",".png"), hab, width = 8, height = 5, units = "in",dpi=300)

### END make daily plots ###

### Start make combinded daily plots ### 

low_act <- cbind(rowSums(low_act_bird_ave[,1:3]),low_act_bird_ave[,4:5])
colnames(low_act) <- c('Lo','d_start', 'w_start')

med_act <- cbind(rowSums(med_act_bird_ave[,1:3]),med_act_bird_ave[,4:5])
colnames(med_act) <- c('Me','d_start', 'w_start')

high_act <- cbind(rowSums(high_act_bird_ave[,1:3]),high_act_bird_ave[,4:5])
colnames(high_act) <- c('Hi','d_start', 'w_start')

act_short <- merge(merge(low_act,med_act),high_act)

act_long <- pivot_longer(act_short,col=c("Lo","Me","Hi"))

act_long$d_start <- as.numeric(high_act_bird_long$d_start)
act_long$w_start <- as.numeric(high_act_bird_long$w_start)
act_long$name <- factor(act_long$name, levels=c("Lo","Me","Hi"))

print("Average of Average daily transitions")
(act <- aggregate(act_long$value, list(act_long$name), FUN=mean) )
print("and median of average daily transitions")
(med_act <- aggregate(act_long$value, list(act_long$name), FUN=median))

act_breaks <- rbind(act_long[1,],act_long[which(act_long$w_start != dplyr::lag(act_long$w_start)),])

ave_act_plot <- ggplot(data=act_long, aes(x=d_start, y=value, group=name)) +
  geom_line()+
  geom_point() +
  theme_bw(base_size=24, base_family='Times New Roman') +  
  #xlab("Age (Weeks)") + ylab("Average Daily Number of Transitions") + 
  scale_x_continuous(breaks=act_breaks$d_start, 
                          labels=as.numeric(act_breaks$w_start)+24) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("",#"Activity Class",
                      values=c("Lo" ="#F8766D","Me"="#00BA38","Hi"="#619CFF"),
                      breaks=c("Hi","Me","Lo"), 
                      labels=c("Hi","Me","Lo"))+
  scale_fill_discrete("",#"Activity Class",
                      breaks=c("Hi","Me","Lo"), 
                      labels=c("Hi","Me","Lo"))+
  #labs(title=paste("Average Daily Transitions (n= ",(length(high_bird_ids)+length(low_bird_ids)+length(med_bird_ids)),")"))+
  ylim(0,50)


ggsave(paste0("../figures/all_day/transition_plots/average_trans_per_day_title",".png"), ave_act_plot, width = 8, height = 5, units = "in",dpi=300)

ave_act_plot_nt <- ggplot(data=act_long, aes(x=d_start, y=value, group=name)) +
  geom_line()+
  geom_point() +
  theme_bw(base_size=24, base_family='Times New Roman') +  
  #xlab("Age (Weeks)") + ylab("Average Daily Number of Transitions") + 
  scale_x_continuous(breaks=act_breaks$d_start, 
                          labels=as.numeric(act_breaks$w_start)+24) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name))+
  scale_color_manual("",#"Activity Class",
                      values=c("Lo" ="#F8766D","Me"="#00BA38","Hi"="#619CFF"),
                      breaks=c("Hi","Me","Lo"), 
                      labels=c("Hi","Me","Lo"))+
  scale_fill_discrete("",#"Activity Class",
                      breaks=c("Hi","Me","Lo"), 
                      labels=c("Hi","Me","Lo"))+
  ylim(0,50)


ggsave(paste0("../figures/all_day/transition_plots/average_trans_per_day",".png"), ave_act_plot_nt, width = 8, height = 5, units = "in",dpi=300)

### END make combinded daily plots ### 

