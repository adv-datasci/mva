Sys.setenv(TZ = "EST")
library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "MVA Recommendation"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Instruction", tabName = "instruction", icon = icon("info-circle")),
    menuItem("Recommended MVA Office", icon = icon("th"), tabName = "recommendation"),
    menuItem("Wait Time Prediction", icon = icon("line-chart"), tabName = "prediction")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "instruction", 
            h3("Description"),
            "This Shiny app takes a users location and a specific time and date and lists which Maryland MVA office  is best for them to visit to take care of their business as early as possible. This app incorporates travel distance and time between the users location and MVA offices and wait times for services offered at each of the MVA offices.",
            h3("Methods"),
            em("Data"),
            br(),
            br(),
            "The MVA of Maryland posts wait times every 5 minutes while the locations are open. Using R, we scraped wait times for each service at each MVA office for 4 weeks from November 9, 2017 to December 6, 2017. Data scraped on holidays (Veteran’s Day: November 10-11, 2017 and Thanksgiving day: November 23), were excluded from prediction modeling.",
            br(),
            br(),
            em("Prediction Model"),
            br(),
            br(),
            "The prediction model takes in user’s address, expected leaving date and time, and gives user the suggested MVA office to go where user can start his/her service first compared to other MVA offices. We use the wait times at each MVA office between November 9, 2017 and December 6, 2017 as our training data in a local polynomial regression and makes predictions on expected wait time for future dates at different time slots. Our App takes account of actual travel time from user's location to MVA offices using Google Map API, predicts wait time at each MVA office for the planned service based on user's expected arrival time, and then reports the expected service start time at the suggested MVA office where both travel time and wait time are minimized. Note that our App only reports the suggested MVA office to user when the user’s expected service start time is before the office closes.",
            h3("Instructions"),
            "1. Please type in your street address, city, and zip code (user location/departure address)",
            br(),
            "2. Please select your mode of transportation and service you would like to get at an MVA office.",
            br(),
            "3. Please type in the date of visit in the format of yyyy-mm-dd.",
            br(),
            "4. Please select the time of departure.",
            br(),
            "5. Click “Update” and wait for a few seconds to process your request.",
            h3("Notes"),
            "According to the Maryland MVA website (http://www.mva.maryland.gov/sebin/customerwaittimes/), the MVA's busiest days are at the end of each month and the first days of the next month, so MVA strongly recommends that you plan your visit during the middle of the month. In addition, MVA expect high volume days on Mondays, Fridays, and Saturdays, so MVA recommends that you plan your visit on a Tuesday, Wednesday, or Thursday to minimize your time at MVA.",
            h3("Contributors to this App"),
            "Su Jin Lim, Siruo Wang, Yeya Zheng, Jing Li, Chih-Kai Chang and Feiyang Zheng",
            br(),
            "This app was created for the Advanced Data Science II class (2nd term, 2017) at the Johns Hopkins Bloomberg School of Public Health",
            br(),
            a("Github Link", href = "https://github.com/adv-datasci/mva/")),
    tabItem(tabName = "recommendation", 
            fluidRow(column(width = 8, 
                            box(width = NULL, solidHeader = TRUE, h3("Our Recommendation"), 
                                textOutput("office"),  leafletOutput("id.distPlot1", height = 550)), 
                            box(width = NULL, status = "warning", h3("Estimated Travel Time and Distance From Your Location to MVA Offices"),
                                DT::dataTableOutput("gmapresultTable"))), 
                     column(width = 3, box(width = NULL, status = "warning", title="User Information", 
                                           textInput(inputId="id.street", label="Street", value = "1 W Pratt St"),
                                           textInput(inputId="id.city", label="City", value = "Baltimore"),
                                           textInput(inputId="id.zipcode", label="Zipcode", value = "21201"),
                                           selectInput(inputId = "id.travel.method",
                                                       label = "Select Mode of Travel",
                                                       choices = c("Driving","Walking","Bicycling","Transit")),
                                           selectInput(inputId = "id.visit.reason",
                                                       label = "Select Service",
                                                       choices = c("Driver License Renewal", "Insurance Compliance Division", "Learners Permit",
                                                                   "Other Drivers Services", "Other Vehicle Services", "Registration Renewal", 
                                                                   "Tag Return","Title")),
                                           textInput(inputId = "id.date", label="Date of Visit", value = "2018-01-01"),
                                           
                                           
                                           uiOutput('slider1'),
                                           # sliderInput(inputId = "id.time",
                                           #             label = "Select Time",
                                           #             min = as.POSIXct("08:30:00", format = "%H:%M:%S"),
                                           #             max = as.POSIXct("16:30:00", format = "%H:%M:%S"),
                                           #             value = as.POSIXct("09:00:00", format = "%H:%M:%S"),
                                           #             step = 300,
                                           #             timeFormat = "%H:%M:%S", ticks = F, animate = T),
                                           submitButton("Update")
                                           )
                            
                            )
                     )), 
    tabItem(tabName = "prediction",  
            h3("Wait Time Trend at the Recommended Office"),plotOutput(outputId = "id.distPlot2"))
  )
)
    
    

dashboardPage(
  skin = "yellow",
  header,
  sidebar,
  body
)
