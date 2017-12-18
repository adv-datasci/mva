# setwd("/Users/sara.wang/Documents/datascience/mva_datascience/")
# mvadata <- read.table("mvadata.csv", header = TRUE, sep = ",")
# save(mvadata, file = "mvadata.rda", compress = TRUE)

library(dplyr)
library(ggplot2)
library(kimisc)
library(lubridate)
load("mvadata.rda")
tz(mvadata$date)

wait_time_trend <- function(office, service, day, time_index, arrival_time){

  subset_idx <- which(mvadata$office == office & mvadata$service == service & mvadata$day == day) 
  subset_data <- mvadata[subset_idx,]
  
  model.lo <- loess(wait_time ~ index, subset_data,span=0.2)
  max_idx <- max(subset_data$index)
  min_idx <- min(subset_data$index)
  prediction_curve <- predict(model.lo, data.frame(index = seq(min_idx, max_idx, 0.05)), se = TRUE)
  # plot(seq(1,95,0.1),prediction$fit)
  # points(subset_data$index,subset_data$wait_time,col="red")
  
  expect_wait <- predict(model.lo, data.frame(index = time_index), se = TRUE)$fit
  if (expect_wait < 0){expect_wait <- 0}
  
  subset_dataframe <- data.frame(timepoints = seq(min_idx,max_idx,0.05), predict_wait = prediction_curve$fit)
  
  p <- ggplot(data = subset_dataframe, aes(x=timepoints, y=predict_wait)) + 
    geom_point(size = 0.2) + 
    labs(title = paste0("wait time trend for service ", service," at ", office, " office")) +
    scale_x_continuous(name = "Arrival Time") +
    scale_y_continuous(name = "expected wait time") + 
    geom_segment(aes(x = time_index, y = 0, xend = time_index, yend = expect_wait), linetype="dotted", color = "blue", size=1) +
    geom_segment(aes(x = 0, y = expect_wait, xend = time_index, yend = expect_wait), linetype="dotted", color = "blue", size=1) +
    geom_segment(aes(x = time_index + 5, y = expect_wait + 2, xend = time_index - 0.3, yend = expect_wait + 0.2),
                 arrow = arrow(length = unit(0.5, "cm")), color = "blue") +
    annotate("text", x = 3, y = expect_wait + 0.8, label= paste0(round(expect_wait, digits = 1), " minutes")) + 
    annotate("text", x = time_index, y = -0.8, label= paste0(arrival_time))

  return(p)
}

choose_office <- function(office, service, day, arrival_time){
  result <- list()
  
  arrival_time_seconds <- hms.to.seconds(arrival_time)+1-hms.to.seconds("08:30:00")
  time_index <- ceiling(arrival_time_seconds/300)
  
  wait_time_list <- c()
  for (i in c(1:length(office))){

    subset_idx <- which(mvadata$office == office[i] & mvadata$service == service & mvadata$day == day) 
    subset_data <- mvadata[subset_idx,]
    if (nrow(subset_data)==0){
      expect_wait <- NA
    }else{
      model.lo <- loess(wait_time ~ index, subset_data,span=0.2)
      max_idx <- max(subset_data$index)
      min_idx <- min(subset_data$index)
      prediction_curve <- predict(model.lo, data.frame(index = seq(min_idx, max_idx, 0.05)), se = TRUE)
  
      expect_wait <- predict(model.lo, data.frame(index = time_index[i]), se = TRUE)$fit
      if (!is.na(expect_wait) & expect_wait < 0){expect_wait <- 0}
    }
    
    wait_time_list <- c(wait_time_list, expect_wait)
  }
  
  service_starttime_seconds <- arrival_time_seconds + wait_time_list * 60
  if (which(!is.na(service_starttime_seconds)) %>% length() == 0){
    result <- NA
  }else{
    choose_idx <- which(service_starttime_seconds == min(service_starttime_seconds[!is.na(service_starttime_seconds)]))
    if (service_starttime_seconds[choose_idx] < hms.to.seconds("16:30:00")+1 - hms.to.seconds("08:30:00")){
      result[[1]] <- office[choose_idx] ## predicted office to go
      result[[2]] <- time_index[choose_idx] ## office (predicted) arrival time index
      result[[3]] <- arrival_time[choose_idx] ## office (predicted) actual arrival time
      result[[4]] <- wait_time_list[choose_idx] ## expected wait time in the predicted office
      result[[5]] <- seconds.to.hms(service_starttime_seconds[choose_idx] + hms.to.seconds("08:30:00")) ## expected time to start the service in that office
    }else{result <- NA}
  }
  return(result)
}

#######################################################################
## function outputs testing

#output.data <- feiyang()
# load("sample output.RData")
# output.data2 = est.time.distance(address="929 North Wolfe Street, MD 21209",commute="driving",dep.date="2017-12-31",dep.time="12:40:00")
# office <- levels(droplevels(output.data2$office))
# office <- as.character(output.data2$office)
# service <- "Title"
# day <- output.data2$day[1] 
# arrival_time <- output.data2$arr.time
# 
# predict_result <- choose_office(office, service, day, arrival_time)
# print(predict_result)
# 
# shiny_output <- output.data[output.data$office == predict_result[[1]],]
# shiny_output
# shiny_output$est.time %>% seconds_to_period()
#   
# office <- predict_result[[1]]
# time_index <- predict_result[[2]]
# arrival_time <- predict_result[[3]]
# graph <- wait_time_trend(office, service, day, time_index, arrival_time)
# print(graph)
 