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
library(sna)

# read in room 2 tables
library(readr)
library(ggplot2)

tagnames <- FALSE

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

rm_2_hr[is.na(rm_2_hr)] <- 0

rm_2_hr_median<-ifelse(rm_2_hr>=rm_2_med, 1, 0)

png("../figures/all_day/rm_2_median_heatmap.png")
heatmap(rm_2_hr_median, Colv =NA, symm =TRUE, revC = TRUE)
dev.off()


rm_2_net <- network::network(rm_2_hr_median, directed=F)
# color based on activity classification 

rm_2_act_class_quart <- read.csv("../intermediate/rm_2_activity_class.csv")

rm_2_act_class_quart <- rm_2_act_class_quart[match(colnames(rm_2_hr), rm_2_act_class_quart$tagname),]
rm_2_act_class_quart$color <- ifelse(rm_2_act_class_quart$activity=="low","#F8766D", ifelse(rm_2_act_class_quart$activity=="medium","#00BA38","#619CFF"))
#rm_2_act_class_quart$color <- ifelse(rm_2_act_class_quart$activity=="low","red", ifelse(rm_2_act_class_quart$activity=="medium","green","blue"))

png("../figures/all_day/rm_2_median_netmap_quart.png")
plot(rm_2_net, main="Room 2 Social Network Quartile", displaylabels=tagnames, vertex.col=rm_2_act_class_quart$color)
legend("topleft",
       legend = c("low","medium","high"),
       fill = c("#F8766D","#00BA38","#619CFF"),       # Color of the squares
       border = "black") # Color of the border of the squares
dev.off()

rm_2.degree <- sna::degree(rm_2_net, gmode="graph")
rm_2.closeness <- sna::closeness(rm_2_net, gmode="graph")
rm_2.betweenness <- sna::betweenness(rm_2_net, gmode="graph")

rm_2_all <- data.frame(network.vertex.names(rm_2_net),rm_2.degree,rm_2.closeness,rm_2.betweenness)
colnames(rm_2_all) <- c("tagname","degree","closeness","betweenness")
rm_2_all <- merge(rm_2_act_class_quart[,c(1,3)],rm_2_all)

(rm_2_all.low <- summary(rm_2_all[rm_2_all$activity == "low",][,c(3:5)]))
(rm_2_all.med <-summary(rm_2_all[rm_2_all$activity == "medium",][,c(3:5)]))
(rm_2_all.high <-summary(rm_2_all[rm_2_all$activity == "high",][,c(3:5)]))
(rm_2_all.overall <- summary(rm_2_all[,c(3:5)]))

png("../figures/all_day/rm_2_graph_characteristics.png")
{par(mfrow=c(1,3))
  hist(rm_2.degree, main="Room 2 degree",xlab=NA,ylab=NA)
  hist(rm_2.closeness,main="Room 2 closeness", xlab=NA, ylab=NA)
  hist(rm_2.betweenness, main="Room 2 betweenness", xlab=NA, ylab=NA)
}
dev.off()

# read in room 3 tables

rm_3_records <- Sys.glob("../intermediate/all_rooms/room_3_tsibble_*")

rm_3 <- rm_3_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")

rm_3_adj <- makeAdjMat(rm_3)

rm_3_hr <- (((rm_3_adj*5)/60)/60)

(fivenum(na.exclude(rm_3_hr[rm_3_hr < 1386])))

rm_3_med <- median(na.exclude(rm_3_hr[rm_3_hr < 1386]))
# number of hours for the light off period 6 hours 22:00-04:00 * 59 days of study

rm_3_hr[is.na(rm_3_hr)] <- 0

rm_3_hr_median<-ifelse(rm_3_hr>=rm_3_med, 1, 0)

png("../figures/all_day/rm_3_median_heatmap.png")
heatmap(rm_3_hr_median, Colv =NA, symm =TRUE, revC = TRUE)
dev.off()


rm_3_net <- network::network(rm_3_hr_median, directed=F)
# color based on activity classification 

rm_3_act_class_quart <- read.csv("../intermediate/rm_3_activity_class.csv")

rm_3_act_class_quart <- rm_3_act_class_quart[match(colnames(rm_3_hr), rm_3_act_class_quart$tagname),]
rm_3_act_class_quart$color <- ifelse(rm_3_act_class_quart$activity=="low","#F8766D", ifelse(rm_3_act_class_quart$activity=="medium","#00BA38","#619CFF"))
#rm_3_act_class_quart$color <- ifelse(rm_3_act_class_quart$activity=="low","red", ifelse(rm_3_act_class_quart$activity=="medium","green","blue"))

png("../figures/all_day/rm_3_median_netmap_quart.png")
plot(rm_3_net, main="Room 3 Social Network Quartile", displaylabels=tagnames, vertex.col=rm_3_act_class_quart$color)
legend("topleft",
       legend = c("low","medium","high"),
       fill = c("#F8766D","#00BA38","#619CFF"),       # Color of the squares
       border = "black") # Color of the border of the squares
dev.off()

rm_3.degree <- sna::degree(rm_3_net, gmode="graph")
rm_3.closeness <- sna::closeness(rm_3_net, gmode="graph")
rm_3.betweenness <- sna::betweenness(rm_3_net, gmode="graph")

rm_3_all <- data.frame(network.vertex.names(rm_3_net),rm_3.degree,rm_3.closeness,rm_3.betweenness)
colnames(rm_3_all) <- c("tagname","degree","closeness","betweenness")
rm_3_all <- merge(rm_3_act_class_quart[,c(1,3)],rm_3_all)

(rm_3_all.low <- summary(rm_3_all[rm_3_all$activity == "low",][,c(3:5)]))
(rm_3_all.med <-summary(rm_3_all[rm_3_all$activity == "medium",][,c(3:5)]))
(rm_3_all.high <-summary(rm_3_all[rm_3_all$activity == "high",][,c(3:5)]))
(rm_3_all.overall <- summary(rm_3_all[,c(3:5)]))

png("../figures/all_day/rm_3_graph_characteristics.png")
{par(mfrow=c(1,3))
  hist(rm_3.degree, main="Room 3 degree",xlab=NA,ylab=NA)
  hist(rm_3.closeness,main="Room 3 closeness", xlab=NA, ylab=NA)
  hist(rm_3.betweenness, main="Room 3 betweenness", xlab=NA, ylab=NA)
}
dev.off()

# read in room 8 tables

rm_8_records <- Sys.glob("../intermediate/all_rooms/room_8_tsibble_*")

rm_8 <- rm_8_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")

rm_8_adj <- makeAdjMat(rm_8)

rm_8_hr <- (((rm_8_adj*5)/60)/60)

(fivenum(na.exclude(rm_8_hr[rm_8_hr < 1386])))

rm_8_med <- median(na.exclude(rm_8_hr[rm_8_hr < 1386]))
# number of hours for the light off period 6 hours 22:00-04:00 * 59 days of study

rm_8_hr[is.na(rm_8_hr)] <- 0

rm_8_hr_median<-ifelse(rm_8_hr>=rm_8_med, 1, 0)

png("../figures/all_day/rm_8_median_heatmap.png")
heatmap(rm_8_hr_median, Colv =NA, symm =TRUE, revC = TRUE)
dev.off()


rm_8_net <- network::network(rm_8_hr_median, directed=F)
# color based on activity classification 

rm_8_act_class_quart <- read.csv("../intermediate/rm_8_activity_class.csv")

rm_8_act_class_quart <- rm_8_act_class_quart[match(colnames(rm_8_hr), rm_8_act_class_quart$tagname),]
rm_8_act_class_quart$color <- ifelse(rm_8_act_class_quart$activity=="low","#F8766D", ifelse(rm_8_act_class_quart$activity=="medium","#00BA38","#619CFF"))
#rm_8_act_class_quart$color <- ifelse(rm_8_act_class_quart$activity=="low","red", ifelse(rm_8_act_class_quart$activity=="medium","green","blue"))

png("../figures/all_day/rm_8_median_netmap_quart.png")
plot(rm_8_net, main="Room 8 Social Network Quartile", displaylabels=tagnames, vertex.col=rm_8_act_class_quart$color)
legend("topleft",
       legend = c("low","medium","high"),
       fill = c("#F8766D","#00BA38","#619CFF"),       # Color of the squares
       border = "black") # Color of the border of the squares
dev.off()

rm_8.degree <- sna::degree(rm_8_net, gmode="graph")
rm_8.closeness <- sna::closeness(rm_8_net, gmode="graph")
rm_8.betweenness <- sna::betweenness(rm_8_net, gmode="graph")

rm_8_all <- data.frame(network.vertex.names(rm_8_net),rm_8.degree,rm_8.closeness,rm_8.betweenness)
colnames(rm_8_all) <- c("tagname","degree","closeness","betweenness")
rm_8_all <- merge(rm_8_act_class_quart[,c(1,3)],rm_8_all)

(rm_8_all.low <- summary(rm_8_all[rm_8_all$activity == "low",][,c(3:5)]))
(rm_8_all.med <-summary(rm_8_all[rm_8_all$activity == "medium",][,c(3:5)]))
(rm_8_all.high <-summary(rm_8_all[rm_8_all$activity == "high",][,c(3:5)]))
(rm_8_all.overall <- summary(rm_8_all[,c(3:5)]))

png("../figures/all_day/rm_8_graph_characteristics.png")
{par(mfrow=c(1,3))
  hist(rm_8.degree, main="Room 8 degree",xlab=NA,ylab=NA)
  hist(rm_8.closeness,main="Room 8 closeness", xlab=NA, ylab=NA)
  hist(rm_8.betweenness, main="Room 8 betweenness", xlab=NA, ylab=NA)
}
dev.off()


# read in room 11 tables

rm_11_records <- Sys.glob("../intermediate/all_rooms/room_11_tsibble_*")

rm_11 <- rm_11_records%>% 
  lapply(read_csv) %>%
  lapply(data.table) %>%
  lapply(select,!tagname) %>%
  reduce(merge, by = "datetime")

rm_11_adj <- makeAdjMat(rm_11)

rm_11_hr <- (((rm_11_adj*5)/60)/60)

(fivenum(na.exclude(rm_11_hr[rm_11_hr < 1386])))

rm_11_med <- median(na.exclude(rm_11_hr[rm_11_hr < 1386]))
# number of hours for the light off period 6 hours 22:00-04:00 * 59 days of study

rm_11_hr[is.na(rm_11_hr)] <- 0

rm_11_hr_median<-ifelse(rm_11_hr>=rm_11_med, 1, 0)

png("../figures/all_day/rm_11_median_heatmap.png")
heatmap(rm_11_hr_median, Colv =NA, symm =TRUE, revC = TRUE)
dev.off()


rm_11_net <- network::network(rm_11_hr_median, directed=F)
# color based on activity classification 

rm_11_act_class_quart <- read.csv("../intermediate/rm_11_activity_class.csv")

rm_11_act_class_quart <- rm_11_act_class_quart[match(colnames(rm_11_hr), rm_11_act_class_quart$tagname),]
rm_11_act_class_quart$color <- ifelse(rm_11_act_class_quart$activity=="low","#F8766D", ifelse(rm_11_act_class_quart$activity=="medium","#00BA38","#619CFF"))
#rm_11_act_class_quart$color <- ifelse(rm_11_act_class_quart$activity=="low","red", ifelse(rm_11_act_class_quart$activity=="medium","green","blue"))

png("../figures/all_day/rm_11_median_netmap_quart.png")
plot(rm_11_net, main="Room 11 Social Network Quartile", displaylabels=tagnames, vertex.col=rm_11_act_class_quart$color)
legend("topleft",
       legend = c("low","medium","high"),
       fill = c("#F8766D","#00BA38","#619CFF"),       # Color of the squares
       border = "black") # Color of the border of the squares
dev.off()

rm_11.degree <- sna::degree(rm_11_net, gmode="graph")
rm_11.closeness <- sna::closeness(rm_11_net, gmode="graph")
rm_11.betweenness <- sna::betweenness(rm_11_net, gmode="graph")

rm_11_all <- data.frame(network.vertex.names(rm_11_net),rm_11.degree,rm_11.closeness,rm_11.betweenness)
colnames(rm_11_all) <- c("tagname","degree","closeness","betweenness")
rm_11_all <- merge(rm_11_act_class_quart[,c(1,3)],rm_11_all)

(rm_11_all.low <- summary(rm_11_all[rm_11_all$activity == "low",][,c(3:5)]))
(rm_11_all.med <-summary(rm_11_all[rm_11_all$activity == "medium",][,c(3:5)]))
(rm_11_all.high <-summary(rm_11_all[rm_11_all$activity == "high",][,c(3:5)]))
(rm_11_all.overall <- summary(rm_11_all[,c(3:5)]))

png("../figures/all_day/rm_11_graph_characteristics.png")
{par(mfrow=c(1,3))
  hist(rm_11.degree, main="Room 11 degree",xlab=NA,ylab=NA)
  hist(rm_11.closeness,main="Room 11 closeness", xlab=NA, ylab=NA)
  hist(rm_11.betweenness, main="Room 11 betweenness", xlab=NA, ylab=NA)
}
dev.off()