#### All Room: 2,3,8,11 Dynamic Time Warping Analysis ####

#source("./rfid_functions.R")

# Generate Transition tables from Room 11
library(xts)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tsibble)
library(testthat)
library(data.table)
library(network)
library(sna)

# # read in room 2 tables
library(readr)
library(ggplot2)
library(dtwclust)

library(LTS)

rm_2_records <- Sys.glob("../intermediate/all_rooms/room_2_tsibble_*")

rm_2 <- rm_2_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")

rm_2 <- data.frame(rm_2[,-1])

rm_2[rm_2 == "bottom"] <- 1
rm_2[rm_2 == "middle"] <- 2
rm_2[rm_2 == "top"] <- 3

cluster_dtw_h2 <- dtwclust::tsclust(tslist(t(rm_2)), type = "h", k = 2L,
                                    preproc = zscore,
                                    distance = "dtw", centroid = shape_extraction,
                                    control = hierarchical_control(method = "complete"))


# read in room 3 tables

rm_3_records <- Sys.glob("../intermediate/all_rooms/room_3_tsibble_*")

rm_3 <- rm_3_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")


# read in room 8 tables

rm_8_records <- Sys.glob("../intermediate/all_rooms/room_8_tsibble_*")

rm_8 <- rm_8_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")

# read in room 11 tables

rm_11_records <- Sys.glob("../intermediate/all_rooms/room_11_tsibble_*")

rm_11 <- rm_11_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")