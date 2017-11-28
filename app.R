#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Maryland MVA"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(

         textInput(inputId="id.street", label="Street", value = "Street Address:"),
         textInput(inputId="id.city", label="City", value = ""),
         textInput(inputId="id.zipcode", label="Zipcode", value = ""),
         
         selectInput(inputId = "id.travel.method",
                     label = "Select Your Traveling Method",
                     choices = c("driving","walking","bicycling","transit")),
         
        
         selectInput(inputId = "id.visit.reason",
                     label = "Select the visit reason",
                     choices = c("DriverLicenseRenewal", "InsuranceComplianceDivision", "LearnersPermit",
                                 "OtherDriversServices", "OtherVehicleServices", "RegistrationRenewal", 
                                 "TagReturn","Title")),
         
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
                     tabPanel("plot1", plotOutput(outputId = "id.distPlot1")),
                     tabPanel("plot2", plotOutput(outputId = "id.distPlot2")))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  setwd("/Users/yeyazheng/Desktop/327")
   library(dplyr)
   library(ggplot2)
   library(gmapsdistance)
   library(ggmap)
   geo.office = read.csv("./mva_data.csv")
   des<-paste(geo.office$lat , geo.office$lon,sep = "+")
   mvadata=read.csv("./mvadata.csv")%>%mutate(weekday=weekdays(as.Date(as.character(date),"%Y-%m-%d")))
   
   #user_address= reactive ({geocode(paste(paste(input$id.street,input$id.city,"MD",sep=","),input$id.zipcode))})#paste user input address and find its lattitude &longitude
  # ori<-paste(user_address$lat, user_address$lon,sep = "+")
  #disinfor<-reactive({gmapsdistance(origin = ori,destination = des %>% as.vector(),
           #mode = input$id.travel.method, 
            #shape = "long")})#compute the distance from origin to all the office locations
  #des_closest=geo.office[which.min(disinfor()$Distance$Distance),]#find out the closest office
  

   output$id.distPlot1 <- renderPlot({
     
      sub.mvadata = mvadata %>%
        filter(office == as.character(des_closest$Name )& visit.reason == gsub("[[:space:]]", "",input$id.visit.reason) & day == input$id.day) %>%
        group_by(time) %>%
        summarise(mean.wait.people = mean(wait.people),
                  mean.wait.time = mean(wait.time)) %>%
        as.data.frame()
     
      qplot(sub.mvadata$time, sub.mvadata$mean.wait.people, 
           col = 'darkgray', border = 'white',
           main = "Histogram of Average Number of Waiting People",
           xlab = "Time", ylab = "Average Number of Waiting People")

   })
   
   output$id.distPlot2 <- renderPlot({
     
      sub.mvadata = mvadata %>%
        filter(office == as.character(des_closest$Name ) & visit.reason == gsub("[[:space:]]", "",input$id.visit.reason) & day == input$id.day)%>%
        group_by(time) %>%
        summarise(mean.wait.people = mean(wait.people),
                  mean.wait.time = mean(wait.time)) %>%
        as.data.frame()
     
      hist(sub.mvadata$mean.wait.time, 
           col = 'darkgray', border = 'white',
           main = "Histogram of Average Waiting Time",
           xlab = "Average Waiting Time", ylab = "Frequncy")
      
   })}
   


# Run the application 
shinyApp(ui = ui, server = server)

