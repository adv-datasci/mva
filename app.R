

library(shiny)

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
                  label = "Select Your Traveling Method",
                  choices = c("driving","walking","bicycling","transit")),
      
      
      selectInput(inputId = "id.visit.reason",
                  label = "Select the visit reason",
                  choices = c("Driver License Renewal", "Insurance Compliance Division", "Learners Permit",
                              "Other Drivers Services", "Other Vehicle Services", "Registration Renewal", 
                              "Tag Return","Title")),
      
      selectInput(inputId = "id.day",
                  label = "Select the day",
                  choices = c("Monday","Tuesday","Wednesday","Thursday","Friday"))
      ,
      sliderInput(inputId="id.time", 
                  label="Select Time", 
                  min = as.POSIXct("08:00:00", format = "%H:%M:%S"),
                  max = as.POSIXct("17:00:00", format = "%H:%M:%S"),
                  value =as.POSIXct("08:00:00", format = "%H:%M:%S"),
                  step=900,
                  timeFormat = "%H:%M:%S", ticks = F, animate = T)
      
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tab",
                  tabPanel("plot1",h3("Closest office from your address:"),textOutput("office"),plotOutput(outputId = "id.distPlot1")),
                  tabPanel("plot2", plotOutput(outputId = "id.distPlot2")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
<<<<<<< HEAD
  setwd("/Users/yeyazheng/Desktop/327")
   library(dplyr)
   library(ggplot2)
   library(gmapsdistance)
   library(ggmap)
   library(ggalt)
  
   geo.office = read.csv("./mva_data.csv")
   des<-paste(geo.office$lat , geo.office$lon,sep = "+")
   
   closet_office=reactive({
   address<-tryCatch({geocode(paste(paste(input$id.street ,input$id.city,"MD",sep=","),input$id.zipcode))},error=function(e){cat("The input address is not valid,try another one!\n")})#get the user's coordinates
  if(length(address)>0){
    ori=paste(address$lat,address$lon,sep="+")
   mvadata=read.csv("./mvadata.csv")%>%mutate(weekday=weekdays(as.Date(as.character(date),"%Y-%m-%d")))
   disinfor<-gmapsdistance(origin = ori,destination = des %>% as.vector(),
           mode =input$id.travel.method, 
            shape = "long")#compute the distance from origin to all the office locations
  geo.office[which.min(disinfor$Distance$Distance),c("Name","Address")]#find out the closest office
 }  })
   output$office <- renderText({
     a=closet_office()
     paste(a$Name,a$Address)
   })
   output$id.distPlot1 <- renderPlot({
     
     # Get User's Coordinates --------------------------------
     address<-paste(paste(input$id.street ,input$id.city,"MD",sep=","),input$id.zipcode)
     address_loc=tryCatch({geocode(address)},error=function(e){cat("The input address is not valid,try another one!\n")})
     a=closet_office()
     
      if(length(address_loc)>0){
        places_loc <- geocode(as.character(a$Address))  # get longitudes and latitudes
        places_loc<-rbind(address_loc,places_loc)
        qmap(as.character(input$id.city), zoom = 10,source="google",maptype="roadmap")+  geom_point(aes(x=lon, y=lat),
                                                 data =places_loc, 
                                                 alpha = 0.7, 
                                                 size = 2, 
                                                 color = "tomato")+ geom_encircle(aes(x=lon, y=lat),
                                                                                  data = places_loc, size = 2, color = "blue")+geom_text(aes(x=lon, y=lat+0.01),
                                                                                                                                         data = places_loc,label=c("Your Address","Closest MVA office"),cex=1.8,colour="red")
       

       
       }
     
     
     
          
   })
   
   output$id.distPlot2 <- renderPlot({
     office_closest=closet_office()
      sub.mvadata = mvadata %>%
        filter(office == as.character(office_closest$Name) & service == input$id.visit.reason & weekday == input$id.day)%>%
        group_by(time) %>%
        summarise(mean.wait.people = mean(num_people),
                  mean.wait.time = mean(wait_time)) %>%
        as.data.frame()
     
      hist(sub.mvadata$mean.wait.time, 
           col = 'darkgray', border = 'white',
           main = "Histogram of Average Waiting Time",
           xlab = "Average Waiting Time", ylab = "Frequncy")
=======
  library(dplyr)
  library(ggplot2)
  library(gmapsdistance)
  library(ggmap)
  
  setwd("~/Desktop/Data Science Project")
  geo.office = read.csv("./mva_data.csv")
  des<-paste(geo.office$lat , geo.office$lon,sep = "+")
  
  closet_office=reactive({
    address<-tryCatch({geocode(paste(paste(input$id.street ,input$id.city,"MD",sep=","),input$id.zipcode))},error=function(e){cat("The input address is not valid,try another one!\n")})
    if(length(address)>0){
      ori=paste(address$lat,address$lon,sep="+")
      mvadata = 
        read.table("https://raw.githubusercontent.com/adv-datasci/mva/master/mvadata.csv", sep = ",", header = T) %>% 
        mutate(weekday=weekdays(as.Date(as.character(date),"%Y-%m-%d")))
      disinfor <- gmapsdistance(origin = ori,destination = des %>% as.vector(),
                              mode =input$id.travel.method, 
                              shape = "long")#compute the distance from origin to all the office locations
      geo.office[which.min(disinfor$Distance$Distance),c("Name","Address")]#find out the closest office
    }  })
  
  output$office <- renderText({
    a = closet_office()
    paste(a$Name,a$Address)
  })
  
  output$id.distPlot1 <- renderPlot({
    
    # Get User's Coordinates --------------------------------
    address<-paste(paste(input$id.street ,input$id.city,"MD",sep=","),input$id.zipcode)
    address_loc = tryCatch({geocode(address)},error=function(e){cat("The input address is not valid,try another one!\n")})
    a = closet_office()
    
    if(length(address_loc)>0){
>>>>>>> 5e94dae60eec7b4271bb63fb21235fddb90cd445
      
      map1=qmap(address, maprange = TRUE, zoom =13,
                base_layer = ggplot(aes(x=lon, y=lat), data = address_loc)) +
        geom_point()
      
      places_loc <- geocode(as.character(a$Address))  # get longitudes and latitudes
      
      # Plot Open Street Map -------------------------------------
      map1 + geom_point(aes(x=lon, y=lat),
                        data = places_loc, 
                        alpha = 0.7,
                        size = 7)
    }
  })
                        
  
  output$id.distPlot2 <- renderPlot({
    office_closest=closet_office()
    sub.mvadata = mvadata %>%
      filter(office == as.character(office_closest$Name) & service == gsub(" ", "", input$id.visit.reason) & weekday == input$id.day)%>%
      group_by(time) %>%
      summarise(mean.wait.people = mean(num_people),
                mean.wait.time = mean(wait_time)) %>%
      as.data.frame()
    
    hist(sub.mvadata$mean.wait.time, 
         col = 'darkgray', border = 'white',
         main = "Histogram of Average Waiting Time",
         xlab = "Average Waiting Time", ylab = "Frequncy", breaks = 30)
    
  })
  }



# Run the application 
shinyApp(ui = ui, server = server)
