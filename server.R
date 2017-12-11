library(shiny)
library(shinydashboard)
# Define server logic required to draw a plot
shinyServer(function(input, output){
  
  #setwd("/Users/yeyazheng/Desktop/327")
  library(leaflet)
  library(DT)
  library(maps)
  library(dplyr)
  library(ggplot2)
  library(gmapsdistance)
  library(ggmap)
  library(ggalt)
  
  geo.office = read.csv("~/Desktop/Data Science Project/mva_data.csv")
  des<-paste(geo.office$lat , geo.office$lon,sep = "+")
  
  ########################
  ## Find the nearest office
  ########################
  
  user.address = reactive({
    paste(paste(input$id.street ,input$id.city,"MD",sep=", "),input$id.zipcode)
  })
  
  user.address.cord = reactive({
    tryCatch({geocode(user.address(), source = "google")},
             error=function(e){cat("The input address is not valid,try another one!\n")})#get the user's coordinates
  })
  
  gmapresult = reactive({
    if(length(user.address.cord())>0){
      ori = paste(user.address.cord()$lat,user.address.cord()$lon,sep="+")
      mvadata = read.csv("https://raw.githubusercontent.com/adv-datasci/mva/master/mvadata.csv", sep = ",") %>%
        mutate(day = as.character(day),
               service = as.character(service),
               office = as.character(office))
      disinfor<-gmapsdistance(origin = ori, destination = des %>% as.vector(),
                              mode = input$id.travel.method, 
                              shape = "long")#compute the distance from origin to all the office locations
      result = cbind(geo.office$Name, geo.office$Address,
                     disinfor$Status, disinfor$Time$Time, disinfor$Distance$Distance) %>% 
        setNames(c("Office","Address","or","de","status","Time (minutes)","Distance (miles)")) %>%
        mutate(`Time (minutes)` = round(`Time (minutes)`/60, 0), # convert unit from sec to minute
               `Distance (miles)` = round(`Distance (miles)` * 0.000621371,1)) # convert unit from m to mile
      result
    }
  })
  
  closest_office = reactive({
    geo.office[which.min(gmapresult()$`Distance (miles)`),]
  })
  
  
  ########################
  ## Output results
  ########################
  
  # output the nearest office
  output$office <- renderText({
    
    a=closest_office()
    paste(a$Name, a$Address)
    
  })
  
  # output map
  output$id.distPlot1 <- renderLeaflet({
    
    a = closest_office()
    b = user.address()
    c = user.address.cord()
    
    maryland = map("county","maryland",fill=TRUE, plot=FALSE)
    leaflet(data = maryland) %>% 
      addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = 0.1), stroke = TRUE, color = "blue", weight = 2) %>%
      # set default view to Baltimore area
      # setView(lng = -77.03687 , lat = 38.90719, zoom = 8) %>% 
      fitBounds(lng1 = a$lon, lat1 = a$lat, lng2 = c$lon, lat2 = c$lat) %>%
      addAwesomeMarkers(lng = a$lon, lat = a$lat, label = paste(a$Name, "office:", a$Address),
                        icon = makeAwesomeIcon(icon = "flag", markerColor = "black", iconColor = "white"),
                        labelOptions = labelOptions(noHide = T)) %>%
      addAwesomeMarkers(lng = c$lon, lat = c$lat, label = paste("Your Location :", b),
                        icon = makeAwesomeIcon(icon = "home", markerColor = "red", iconColor = "white"),
                        labelOptions = labelOptions(noHide = T))
  })
  
  # output table
  output$gmapresultTable <- DT::renderDataTable({
    a = gmapresult() %>%
      select(Office, Address, `Time (minutes)`, `Distance (miles)`)
    DT::datatable(a)
  })
  
  output$id.distPlot2 <- renderPlot({
    
    office_closest=closest_office()
    
    sub.mvadata = mvadata %>%
      filter(office == as.character(office_closest$Name) & service == gsub(pattern = " ", replacement =  "", x = input$id.visit.reason) & day == input$id.day) %>%
      group_by(index) %>%
      summarise(mean.wait.people = mean(num_people),
                mean.wait.time = mean(wait_time)) %>%
      as.data.frame()
    
    plot(sub.mvadata$index, sub.mvadata$mean.wait.time,
         col = 'darkgray', pch = 16,
         xlab = "Time Index (5 min)", ylab = "Mean Waiting Time (minutes)")
    
  })
  
  
  #  output$id.distPlot1 <- renderPlot({
  # 
  #    # Get User's Coordinates --------------------------------
  #    address<-paste(paste(input$id.street ,input$id.city,"MD",sep=", "),input$id.zipcode)
  #    address_loc=tryCatch({geocode(address,source = "google")},
  #                         error=function(e){cat("The input address is not valid,try another one!\n")})
  #    a=closet_office()
  #    
  #   if(length(address_loc)>0){
  #     places_loc <- data.frame(lon=a$lon,lat=a$lat)  # get longitudes and latitudes
  #     places_loc <- rbind(address_loc,places_loc)
  #     qmap(as.character(input$id.city), zoom = 10,source="google", maptype="roadmap") +
  #       geom_point(aes(x=lon, y=lat), data = places_loc, alpha = 0.7, size = 2, color = "tomato") + 
  #       geom_encircle(aes(x=lon, y=lat), data = places_loc, size = 2, color = "blue") +
  #       geom_text(aes(x=lon, y=lat+0.01), data = places_loc, label=c("Your Address","Closest MVA office"), cex=1.8, colour="red")
  # 
  #      }
  #    
  #  })
  #  
  
  
})
