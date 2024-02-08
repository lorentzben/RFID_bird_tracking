#### All Room: 2,3,8,11 Social Network Analysis ####

source("./rfid_functions.R")

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

# read in room 2 tables
library(readr)
library(ggplot2)

makeAdjMat <- function(record_tab){
  n_tags <- length(colnames(record_tab))-1
  tag_name <- colnames(record_tab)[1:n_tags+1]
  result <- matrix(ncol=n_tags,nrow=n_tags)
  colnames(result) <- tag_name
  rownames(result) <- tag_name

  for(i in 1:n_tags){
    
    for(j in i:n_tags){
     
      rec_i = i+1
      rec_j = j+1
      intersect_res <- sum(record_tab[,..rec_i] == record_tab[,..rec_j])
      result[i,j] <- intersect_res
      
    }
  }

  return(result)
}

rm_2_records <- Sys.glob("../intermediate/all_rooms/room_2_tsibble_*")

rm_2 <- rm_2_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")

# TODO, do this for the other rooms

rm_2_adj <- makeAdjMat(rm_2)

rm_2_hr <- (((rm_2_adj*5)/60)/60)

(fivenum(na.exclude(rm_2_hr[rm_2_hr < 1386])))

rm_2_med <- median(na.exclude(rm_2_hr[rm_2_hr < 1386]))
# number of hours for the light off period 6 hours 22:00-04:00 * 59 days of study
night <- 6*59
rm_2_hr[is.na(rm_2_hr)] <- 0


rm_2_hr_median<-ifelse(rm_2_hr>=rm_2_med, 1, 0)
rm_2_hr_night<-ifelse(rm_2_hr>=night, 1, 0)



png("../figures/all_day/rm_2_median_heatmap.png")
heatmap(rm_2_hr_median, Colv =NA, symm =TRUE, revC = TRUE)
dev.off()


png("../figures/all_day/rm_2_night_heatmap.png")
heatmap(rm_2_hr_night, Colv =NA, symm =TRUE, revC = TRUE)
dev.off()

rm_2_net <- network::network(rm_2_hr_median, directed=F)
# color based on activity classification 

rm_2_act_class_quart <- read.csv("../intermediate/rm_2_activity_class.csv")

rm_2_act_class_quart <- rm_2_act_class_quart[match(colnames(rm_2_hr), rm_2_act_class_quart$tagname),]
#rm_2_act_class_quart$color <- ifelse(rm_2_act_class_quart$activity=="low","#F8766D", ifelse(rm_2_act_class_quart$activity=="medium","#00BA38","#619CFF"))
rm_2_act_class_quart$color <- ifelse(rm_2_act_class_quart$activity=="low","red", ifelse(rm_2_act_class_quart$activity=="medium","green","blue"))

rm_2_act_class_k <- read.csv("../intermediate/rm_2_k_cluster.csv")

rm_2_act_class_3 <- rm_2_act_class_k[match(colnames(rm_2_hr), rm_2_act_class_k$X),]
#rm_2_act_class_3$color <- ifelse(rm_2_act_class_3$cluster_3_id=="3","#F8766D", ifelse(rm_2_act_class_3$cluster_3_id=="2","#00BA38","#619CFF"))
rm_2_act_class_3$color <- ifelse(rm_2_act_class_3$cluster_3_id=="3","red", ifelse(rm_2_act_class_3$cluster_3_id=="2","green","blue"))

rm_2_act_class_5 <- rm_2_act_class_k[match(colnames(rm_2_hr), rm_2_act_class_k$X),]
rm_2_act_class_5$color <- ifelse(rm_2_act_class_5$cluster_5_id=="4","red", ifelse(rm_2_act_class_5$cluster_5_id=="3","orange",ifelse(rm_2_act_class_5$cluster_5_id=="2","yellow",ifelse(rm_2_act_class_5$cluster_5_id=="1","green","blue"))))

png("../figures/all_day/rm_2_median_netmap_quart.png")
plot(rm_2_net, main="Room 2 Social Network Quartile", displaylabels=T, vertex.col=rm_2_act_class_quart$color)
dev.off()

png("../figures/all_day/rm_2_median_netmap_k3.png")
plot(rm_2_net, main="Room 2 Social Network (k=3)", displaylabels=T, vertex.col=rm_2_act_class_3$color)
dev.off()

png("../figures/all_day/rm_2_median_netmap_k5.png")
plot(rm_2_net, main="Room 2 Social Network (k=5)", displaylabels=T, vertex.col=rm_2_act_class_5$color)
dev.off()

rm_2_net_night <- network::network(rm_2_hr_night, directed=F)
# color based on activity classification 


png("../figures/all_day/rm_2_night_netmap_quart.png")
plot(rm_2_net_night, main="Room 2 Social Network Quartile", displaylabels=T,vertex.col=rm_2_act_class_quart$color)
dev.off()

png("../figures/all_day/rm_2_night_netmap_k3.png")
plot(rm_2_net_night, main="Room 2 Social Network (k=3)", displaylabels=T,vertex.col=rm_2_act_class_3$color)
dev.off()

png("../figures/all_day/rm_2_night_netmap_k5.png")
plot(rm_2_net_night, main="Room 2 Social Network (k=5)", displaylabels=T,vertex.col=rm_2_act_class_5$color)
dev.off()

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

