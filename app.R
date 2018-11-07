library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)

recommendation <- read.csv('recommendation.csv', stringsAsFactors = F, header = T)
surveydata <- readxl::read_xlsx('surveydataece (1).xlsx')
logs <- read.csv('logs (1).csv', sep = ";")

# Title of the header
header <- dashboardHeader(title = "Data Analytics")

# Sidebar content
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("All Users", tabName = "allUsers", icon = icon("users", lib = "font-awesome")),
        #menuItem("Plotly", tabName = "x", icon = icon("bar-chart", lib = "font-awesome")),
        #,
        menuItem("Single User", tabName = "singleUser", icon = icon("user", lib = "font-awesome"),
                 selectInput("user", "User:", width = 300, choices=unique(logs$User)),
                 menuSubItem("Dashboard", tabName = "singleUser", icon = icon("dashboard", lib = "font-awesome")),
                 menuSubItem("Map", tabName = "map", icon = icon("map", lib = "font-awesome"))
        )
    )
)

frow1 <- fluidRow(
    valueBoxOutput("value1"),
    valueBoxOutput("value2"),
    valueBoxOutput("value3")
)

frow2 <- fluidRow(
    box(
        title = "Age groups by gender",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("ageByGender", height = "300px")
    ),
    box(
        title = "User status",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput("status", height = "300px")
    )
)

frow21 <- fluidRow(
  box(
    title = "Total number of each mode",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("totalByType")
  ),
  box(
    title = "Cigarette consumption per weekday",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("freqWeekday")
  )
)

frow22 <- fluidRow(
  box(
    title = "Frequency",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("freqTime")
  )
)

frow.map <- fluidRow(
  box(
    title = "Location history",
    status = "primary",
    width = 12,
    solidHeader = TRUE,
    collapsible = FALSE,
    leafletOutput("mymap",height = 600)
  )
)
# Construct the body

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "allUsers", frow1, frow2),
    tabItem(tabName = "singleUser", frow21, frow22),
    tabItem(tabName = "map", frow.map)
  )
)

ui <- dashboardPage(title = "Project", header, sidebar, body, skin = 'red')

# Server
server <- function(input, output){
    total.revenue <- sum(recommendation$Revenue)
    sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value == max(value))
    prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value == max(value))
    logs.filtered <- reactive({
      x <- logs %>% filter(User==input$user)
    })
    pickup_date <- format(as.POSIXct(strptime(logs$Time,"%d/%m/%Y %H:%M",tz="")), format="%d/%m/%Y")
    pickup_time <- format(as.POSIXct(strptime(logs$Time,"%d/%m/%Y %H:%M",tz="")), format="%H:%M")
    logs$Date <- pickup_date
    logs$OnlyTime <- pickup_time
    logs$Weekday <- weekdays(as.Date(logs$Date,format="%d/%m/%Y"))
    surveydata$Gender <- as.factor(surveydata$Gender)
    types <- count(logs, `Type`)
    ordered <- types[with(types, order(-n)), ]
    users <- count(logs, User)
    total_of_users <- nrow(users)
    surveydata_users <- nrow(surveydata)
    
    # valueBoxOutput content
    output$value1 <- renderValueBox({
        infoBox(
            "Top mode",
            ordered$Type[1],
            icon = icon("list-ol", lib = "font-awesome"),
            color = 'olive')
    })

    output$value2 <- renderValueBox({
        infoBox(
            "Number of users",
            total_of_users,
            icon = icon("users", lib = "font-awesome"),
            color = "aqua")
    })

    output$value3 <- renderValueBox({
        infoBox(
            "Users that didn't answer the survey",
            (total_of_users - surveydata_users),
            icon = icon("exclamation", lib = "font-awesome"),
            color = "teal")
    })

    # plotOutput content
    output$ageByGender <- renderPlot({
        ggplot(data = surveydata,
               aes(x = Age)) + 
               geom_histogram() +
               facet_wrap(~Gender) +
               ylab("# of people") +
               xlab("Age") +
               theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "bold")) +
               ggtitle("Age groups by gender") + labs(fill = "Region")
    })

    output$status <- renderPlotly({
        famStatus <- table(surveydata$'Family status', surveydata$'Gender')
        dataFrame <- as.data.frame(famStatus)
        statuses <- c("Married", "Single")
        female <- dataFrame %>% filter(Var2 == 'Female')
        male <- dataFrame %>% filter(Var2 == 'Male')
        FEM_Cont <- female$Freq
        MAL_Cont <- male$Freq

        p <- plot_ly(dataFrame, x = ~statuses, y = ~FEM_Cont, type = 'bar', marker = list(color = 'rgb(240, 98, 146)'), name = 'Female') %>%
        add_trace(y = ~MAL_Cont, marker = list(color = 'rgb(72, 133, 237)'), name = 'Male') %>%
        layout(yaxis = list(title = 'Count'), barmode = 'group')
        
    })
    
    output$totalByType <- renderPlotly({
      logs <- logs.filtered()
      plot_ly(x = logs$Type, type = "histogram")
    })
    
    #cigarette consumption per weekday
    output$freqWeekday <- renderPlotly({
      logs <- logs.filtered()
      df <- count(logs, 'Weekday')
      #cat(file=stderr(),df)
      df$Weekday <- factor(df$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      plot_ly(data = df, x = df$Weekday, y = df$freq, type = 'bar')
    })
    
    #frequency graph by time
    output$freqTime <- renderPlotly({
      logs <- logs.filtered()
      df <- count(logs, 'Date')
      df$newdate <- strptime(as.character(df$Date), "%d/%m/%Y")
      df$plotlydate <- format(df$newdate, "%Y-%m-%d")
      plot_ly(df, x = df$plotlydate, y = df$freq, mode = 'lines')
    })
    
    #single user map
    getColor <- function(logs) {
      sapply(logs$Type, function(Type) {
        if(Type == "Behaviour") {
          "orange"
        } else if(Type == "Friend") {
          "gray"
        } else if(Type == "Auto skipped") {
          "white"
        } else if(Type == "Snoozed") {
          "yellow"
        } else if(Type == "On time") {
          "blue"
        } else if(Type == "Skipped") {
          "green"
        } else {
          "red"
        }
      })
    }
    
    icons <- awesomeIcons(
      icon = 'fire',
      iconColor = 'black',
      library = 'fa',
      markerColor = getColor(logs)
    )
    
    output$mymap <- renderLeaflet({
      logs <- logs.filtered()
      
      m <- leaflet(data = logs) %>%
        addTiles() %>%
        addAwesomeMarkers(lng = logs$Longitude,
                   lat = logs$Latitude,
                   label = paste(logs$User, logs$Type, logs$Time, sep = ", "),
                   group = logs,
                   clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto'),
                   icon=icons)
      m
    })
}

shinyApp(ui, server)