# setwd("/Users/sara.wang/Documents/datascience/mva_datascience/")
# mvadata <- read.table("mvadata.csv", header = TRUE, sep = ",")
# save(mvadata, file = "mvadata.rda", compress = TRUE)

library(dplyr)
library(ggplot2)
load("mvadata.rda")

predict_waittime <- function(office, service, day, time_index){

  subset_idx <- which(mvadata$office == office & mvadata$service == service & mvadata$day == day) 
  subset_data <- mvadata[subset_idx,]
  
  model.lo <- loess(wait_time ~ index, subset_data,span=0.2)
  max_idx <- max(subset_data$index)
  min_idx <- min(subset_data$index)
  prediction_curve <- predict(model.lo, data.frame(index = seq(min_idx, max_idx, 0.05)), se = TRUE)
  # plot(seq(1,95,0.1),prediction$fit)
  # points(subset_data$index,subset_data$wait_time,col="red")
  
  subset_dataframe <- data.frame(timepoints = seq(min_idx,max_idx,0.05), predict_wait = prediction_curve$fit)
  p <- ggplot(data = subset_dataframe, aes(x=timepoints, y=predict_wait)) + 
      geom_point() + 
      labs(title = paste0("wait time trend for service ", service," at ", office, " office")) +
      scale_x_continuous(name = "time index") +
      scale_y_continuous(name = "expected wait time") 
  
  expect_wait <- predict(model.lo, data.frame(index = time_index), se = TRUE)$fit
  if (expect_wait < 0){expect_wait <- 0}
  print(p)
  return(expect_wait)
}

# mvadata$office %>% unique()
# mvadata$service %>% unique()

office <- "Annapolis"
service <- "LearnersPermit"
day <- "Wednesday"
time_index <- 40

predict_waittime(office, service, day, time_index)


