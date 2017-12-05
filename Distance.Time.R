library(dplyr)
library(gmapsdistance)
library(ggmap)
library(lubridate)
library(udunits2)

setwd("~/Desktop")
#### Raw data to get Longitude, Latitude
mvadata = read.csv("mva_data.csv")
mvadata$Office<- paste(mvadata$Name,mvadata$County,sep = ', ')
geo.lon.lat<-geocode(as.character(mvadata$Address))
geo.office<-cbind(mvadata,geo.lon.lat)
write.csv(geo.office,"mva data.csv")
###Processed Data

geo.office = read.csv("mva_data.csv")
geo.office$Office<- paste(geo.office$Name,geo.office$County,sep = ', ')

# Input:address 
#Calculate time and distance
#Input : departure time, departure data, commute, address

est.time.distance <-function(address,commute,dep.date,dep.time){
  
  Lat.MVA<-geo.office$lat
  Lon.MVA<-geo.office$lon
  address<-geocode(address)
  
  ori<-paste(address$lat, address$lon,sep = "+")
  des<-paste(Lat.MVA , Lon.MVA,sep = "+")
  
  disinfor<-gmapsdistance(origin = ori,destination = des %>% as.vector(),
                          mode = commute, 
                          dep_date = dep.date, 
                          dep_time = dep.time, 
                          shape = "long")
  
  
  output.data <- geo.office$Office %>% as.data.frame()
  output.data$est.time <- (disinfor$Time[[3]])/60
  output.data$dis <- udunits2::ud.convert(disinfor$Distance[[3]], "m", "km")
}

address="929 North Wolfe Street, MD 21209"
commute="driving"
dep.date = "2021-08-16"
dep.time = "15:20:00"

# from <- c("houston", "houston", "dallas")
# to <- c("waco, texas", "san antonio",
#       "houston")
# mapdist(from, to)
# 
# 
# 
# 
# from <- "houson, texas"
# to <- "waco, texas"
# route_df <- route(from, to, structure = "route")
# qmap("college station, texas", zoom = 8) +
#   geom_path(
#     aes(x = lon, y = lat),  colour = "red", size = 1.5,
#     data = route_df, lineend = "round"
#   )
# 
# qmap("college station, texas", zoom = 6) +
#   geom_path(
#     aes(x = lon, y = lat), colour = "red", size = 1.5,
#     data = route_df, lineend = "round"
#   )
# 
# routeQueryCheck()
# 
