
library(shiny)
library(leaflet)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Maryland MVA"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(inputId="id.street", label="Street", value = ""),
      
      textInput(inputId="id.city", label="City", value = ""),
      
      textInput(inputId="id.zipcode", label="Zipcode", value = ""),
      
      selectInput(inputId = "id.travel.method",
                  label = "Select Mode of Travel",
                  choices = c("driving","walking","bicycling","transit")),
      
      
      selectInput(inputId = "id.visit.reason",
                  label = "Select Service",
                  choices = c("Driver License Renewal", "Insurance Compliance Division", "Learners Permit",
                              "Other Drivers Services", "Other Vehicle Services", "Registration Renewal", 
                              "Tag Return","Title")),
      
      selectInput(inputId = "id.day",
                  label = "Select day",
                  choices = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
      
      sliderInput(inputId = "id.time",
                  label = "Select Time",
                  min = as.POSIXct("08:30:00", format = "%H:%M:%S"),
                  max = as.POSIXct("16:30:00", format = "%H:%M:%S"),
                  value = c(as.POSIXct("10:00:00", format = "%H:%M:%S"), as.POSIXct("14:00:00", format = "%H:%M:%S")),
                  step = 300,
                  timeFormat = "%H:%M:%S", ticks = F, animate = T),

      submitButton("Update")
      
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(
      
      tabsetPanel(type = "tab",
                  tabPanel("plot1", 
                           h3("The nearest office from your location is"), 
                           textOutput("office"),
                           leafletOutput(outputId = "id.distPlot1"),
                           h3("Estimated travel time and distance from your location to MVA offices"),
                           DT::dataTableOutput("gmapresultTable")),
                  tabPanel("plot2",
                           plotOutput(outputId = "id.distPlot2")))
      
    )
    
  )
  
)

# Define server logic required to draw a plot
server <- function(input, output){
  
   #setwd("/Users/yeyazheng/Desktop/327")
   library(leaflet)
   library(DT)
   library(maps)
   library(dplyr)
   library(ggplot2)
   library(gmapsdistance)
   library(ggmap)
   library(ggalt)
  
   geo.office = read.csv("./mva data.csv")
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
       mvadata = read.csv("./mvadata.csv") %>%
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
   
   
  }



# Run the application 
shinyApp(ui = ui, server = server)
