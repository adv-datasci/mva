library(shinydashboard)

header <- dashboardHeader(
  title = "MVA Recommendation"
)

body <- dashboardBody(
  fluidRow(
    column(width = 8,
      box(width = NULL, solidHeader = TRUE,
        leafletOutput("id.distPlot1", height = 500)
      ), 
      box(width = NULL,
          plotOutput("id.distPlot2", height = 500)
          )
    ), 
    column(width = 3, 
           box(width = NULL, status = "warning", title="User Information",
           textInput(inputId="id.street", label="Street", value = ""),
           textInput(inputId="id.city", label="City", value = ""),
           textInput(inputId="id.zipcode", label="Zipcode", value = ""),
           selectInput(inputId = "id.travel.method", label = "Select Mode of Travel", choices = c("driving","walking","bicycling","transit")),
           selectInput(inputId = "id.visit.reason", label = "Select Service", choices = c("Driver License Renewal", "Insurance Compliance Division", "Learners Permit", "Other Drivers Services", "Other Vehicle Services", "Registration Renewal", "Tag Return","Title")),
           selectInput(inputId = "id.day",
                       label = "Select day",
                       choices = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
           sliderInput(inputId = "id.time",
                       label = "Select Time",
                       min = as.POSIXct("08:30:00", format = "%H:%M:%S"),
                       max = as.POSIXct("16:30:00", format = "%H:%M:%S"),
                       value = c(as.POSIXct("10:00:00", format = "%H:%M:%S"), as.POSIXct("14:00:00", format = "%H:%M:%S")),
                                 step = 300, timeFormat = "%H:%M:%S", ticks = F, animate = T)))
  )
)
    
    

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
