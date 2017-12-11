# setwd("/Users/sara.wang/Documents/datascience/mva_datascience/")
# mvadata <- read.table("mvadata.csv", header = TRUE, sep = ",")
# save(mvadata, file = "mvadata.rda", compress = TRUE)

library(dplyr)
library(ggplot2)
load("mvadata.rda")

predict_waittime <- function(office, service, day, time_index){
  result <- list()
  
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
    scale_x_continuous(name = "time index") +
    scale_y_continuous(name = "expected wait time") + 
    geom_segment(aes(x = time_index, y = 0, xend = time_index, yend = expect_wait), linetype="dotted", color = "blue", size=1) +
    geom_segment(aes(x = 0, y = expect_wait, xend = time_index, yend = expect_wait), linetype="dotted", color = "blue", size=1) +
    geom_segment(aes(x = time_index + 5, y = expect_wait + 2, xend = time_index - 0.3, yend = expect_wait + 0.2),
                 arrow = arrow(length = unit(0.5, "cm"))) +
    annotate("text", x = 3, y = expect_wait + 0.8, label= paste0(round(expect_wait, digits = 1), " minutes")) + 
    annotate("text", x = time_index, y = -0.8, label= paste0(time_index))
    

  
  result[[1]] <- p
  result[[2]] <- expect_wait
  
  return(result)
}

# mvadata$office %>% unique()
# mvadata$service %>% unique()

office <- "Annapolis"
service <- "LearnersPermit"
day <- "Wednesday"
time_index <- 35

result <- predict_waittime(office, service, day, time_index)
print(result[[1]])
print(result[[2]])

