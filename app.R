library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(lubridate)
library(tidyr)

recommendation <- read.csv('recommendation.csv', stringsAsFactors = F, header = T)
surveydata <- readxl::read_xlsx('surveydataece (1).xlsx')
logs <- read.csv('logs (1).csv', sep = ";")
logs$User <- gsub("\x8e|\x91l|\x83|\x8f","e",logs$User)

# Title of the header
header <- dashboardHeader(title = "Data Analytics")

# Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("All Users", tabName = "allUsers", icon = icon("users", lib = "font-awesome"),
             menuSubItem("Information", tabName = "au_info", icon = icon("info", lib = "font-awesome")),
             menuSubItem("Dashboard", tabName = "allUsers", icon = icon("dashboard", lib = "font-awesome"))
             ),
    menuItem("Single User", tabName = "singleUser", icon = icon("user", lib = "font-awesome"),
             selectInput("user", "User:", width = 300, choices=unique(logs$User)),
             menuSubItem("Information", tabName = "su_info", icon = icon("info", lib = "font-awesome")),
             menuSubItem("Classic", tabName = "singleClassic", icon = icon("eye", lib = "font-awesome")),
             menuSubItem("Engagement", tabName = "singleEngagement", icon = icon("star", lib = "font-awesome")),
             menuSubItem("Dashboard", tabName = "singleUser", icon = icon("dashboard", lib = "font-awesome")),
             menuSubItem("Week", tabName = "su_week", icon = icon("calendar", lib = "font-awesome")),
             menuSubItem("All Days", tabName = "su_all", icon = icon("calendar-o", lib = "font-awesome")),
             menuSubItem("Map", tabName = "map", icon = icon("map", lib = "font-awesome"))
        )
    )
)

frow1 <- fluidRow(
    valueBoxOutput("value1"),
    valueBoxOutput("value2"),
  valueBoxOutput("value3")
)

frow.au_info_1 <- fluidRow(
  box(
    tags$b("Total Cigarettes Saved:"), textOutput("cigSavedAll", inline = TRUE), br(),
    tags$b("Total Money Saved:"), textOutput("moneySavedAll", inline = TRUE), br(),
    tags$b("Average Cigarettes Saved:"), textOutput("avgCigSaved", inline = TRUE), br(),
    tags$b("Average Money Saved:"), textOutput("avgMoneySaved", inline = TRUE), br()
  )
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

frow3 <- fluidRow(
  box(
    title = "Total number of each mode",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("totalByTypeAll")
  ),
  box(
    title = "Cigarette consumption per weekday",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("freqWeekdayAll")
  )
)

frow.su_info_1 <- fluidRow(
  valueBoxOutput("userGender"),
  valueBoxOutput("userAge"),
  valueBoxOutput("userMarriage")
)

frow.su_info_2 <- fluidRow(
  box(
    tags$b("Age Category:"), textOutput("ageCat", inline = TRUE), br(),
    tags$b("Cigarettes Saved:"), textOutput("cigSaved", inline = TRUE), br(),
    tags$b("Money Saved:"), textOutput("moneySaved", inline = TRUE), br(),
    tags$b("Mean of consumed cigarettes:"), textOutput("avgCigPerDay", inline = TRUE), br(),
    tags$b("Mean of consumed cigarettes in weekdays:"), textOutput("avgCigWeekday", inline = TRUE), br(),
    tags$b("Mean of consumed cigarettes in weekends:"), textOutput("avgCigWeekend", inline = TRUE), br(),
    tags$b("Most Smoking Intensity Slot:"), textOutput("peakTimeSlot", inline = TRUE), br(),
    tags$b("Overall Engagement:"), textOutput("avgEngagement", inline = TRUE), br()
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
    title = "Frequency of usage by mode",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    selectInput("mode", "Mode:", width = 300, choices=unique(logs$Type)),
    plotlyOutput("freqTime")
  )
)

frow.su_week_1 <- fluidRow(
  box(
    title = "Cigarettes per weekday per time slots",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("cigWeekSlot"),
    sliderInput("cigWeekSlotSlider", "Week:", 1, 100, 50)
  ),
  box(
    title = "Comparison of cigarettes consumption between weeks",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("cigAllWeek")
  )
)

frow.su_week_2 <- fluidRow(
  box(
    title = "Mode usage per week",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("modeWeek"),
    sliderInput("modeWeekSlider", "Week:", 1, 100, 50)
  ),
  box(
    title = "Cigarettes consumption per weekday",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("cigWeek"),
    sliderInput("cigWeekSlider", "Week:", 1, 100, 50)
  )
)

frow.su_all_1 <- fluidRow(
  box(
    title = "Cigarettes consumption over all period",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("allUsage"),
    width = 12
  )
)

frow.su_all_2 <- fluidRow(
  box(
    title = "Mode usage over all period",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("allModeUsage"),
    width = 12
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

frowc1 <- fluidRow(
  box(
    tags$b("Consumption:"), textOutput("totalConsumption", inline = TRUE), br(),
    tags$b("Consumption weekends:"), textOutput("weekendConsumption", inline = TRUE), br(),
    tags$b("Consumption weekdays:"), textOutput("weekdayConsumption", inline = TRUE), br(),
    tags$b("Last 7 days consumption:"), textOutput("lastSevenDays", inline = TRUE)
  ),
  box(
    title = "Mean and std cigarrete consumption per weekday",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("meanAndStd")
  )
)

frowe1 <- fluidRow(
  box(
    width=12,
    title = "Engagement per day",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("engagementPerDay")
  )
)

frowe2 <- fluidRow(
  box(
    width=12,
    title = "Engagement per week",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("engagementPerWeek")
  )
)

# Construct the body

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "allUsers", frow1, frow2, frow3),
    tabItem(tabName = "au_info", frow.au_info_1),
    tabItem(tabName = "su_info", frow.su_info_1, frow.su_info_2),
    tabItem(tabName = "singleUser", frow21, frow22),
    tabItem(tabName = "su_week", frow.su_week_1, frow.su_week_2),
    tabItem(tabName = "su_all", frow.su_all_1, frow.su_all_2),
    tabItem(tabName = "map", frow.map),
    tabItem(tabName = "singleClassic", frowc1),
    tabItem(tabName = "singleEngagement", frowe1, frowe2)
  )
)

ui <- dashboardPage(title = "Project", header, sidebar, body, skin = 'red')

# Server
server <- function(input, output, session){
    Sys.setlocale("LC_TIME", "English") # Set days in english
    total.revenue <- sum(recommendation$Revenue)
    sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value == max(value))
    prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value == max(value))
    logs.filtered <- reactive({
      x <- logs %>% filter(User==input$user)
    })
    logs.filterModeAndUser <- reactive({
      x <- logs %>% filter(User==input$user & Type==input$mode)
    })
    logs.filterSmokedAndUser <- reactive({
      x <- logs %>% filter(User==input$user, Type=='Behaviour' | Type=='Cheated' | Type=='On time')
    })
    logs.Smoked <- reactive({
      x <- logs %>% filter(Type=='Behaviour' | Type=='Cheated' | Type=='On time')
    })
    surveydata.filterUser <- reactive({
      x <- surveydata %>% filter(Name==input$user)
    })
    logs.consumptionWeekday <- reactive({
      x <- logs %>% filter(User==input$user, Type=='Cheated' | Type=='On time')
    })
    pickup_date <- format(as.POSIXct(strptime(logs$Time,"%d/%m/%Y %H:%M",tz="")), format="%d/%m/%Y")
    pickup_time <- format(as.POSIXct(strptime(logs$Time,"%d/%m/%Y %H:%M",tz="")), format="%H:%M")
    logs$Date <- pickup_date
    logs$dateFormatted <- as.Date(format(strptime(as.character(logs$Date), "%d/%m/%Y"), "%Y-%m-%d")) #format to "Date" type
    logs$OnlyTime <- pickup_time
    logs$Weekday <- weekdays(as.Date(logs$Date,format="%d/%m/%Y"))
    logs$week <- with(logs, isoweek(logs$dateFormatted))
    surveydata$Gender <- as.factor(surveydata$Gender)
    types <- count(logs, `Type`)
    ordered <- types[with(types, order(-n)), ]
    users <- count(logs, User)
    total_of_users <- nrow(users)
    surveydata_users <- nrow(surveydata)
    daysofweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    timeSlot <- seq(0, 22, by = 2)
    
    userEndDate <- reactive({
      df <- logs.filtered()
      x <- tail(df$dateFormatted, n = 1)
    })
    
    #cig saved SU
    userStartDate <- reactive({
      df <- logs.filtered()
      x <- df$dateFormatted[1]
    })
    
    userBehaviorWeek <- reactive({
      df <- logs.filtered()
      x <- with(df, df[(dateFormatted <= userStartDate() + 7), ])
    })
    
    userOtherWeek <- reactive({
      df <- logs.filtered()
      x <- with(df, df[(dateFormatted > userStartDate() + 7), ])
    })
    
    userBehaviorWeek.consumed <- reactive({ #return single number (count)
      df <- userBehaviorWeek()
      df <- df %>% filter(Type=='Behaviour')
      data <- count(df)
      x <- data$n[1]
    })

    cigSaved <- reactive({
      behaviorConsumption <- userBehaviorWeek.consumed()
      df <- userOtherWeek()
      df <- df %>%
        group_by(week) %>%
        summarise(all = n(),consumed = sum(Type=='Cheated' | Type=='On time'))
      df$saved <- with(df, behaviorConsumption - df$consumed)
    sum(df$saved)
  })
  
    avgCigPerDay <- reactive({
      df <- logs.filtered()
      df <- df %>%
        group_by(dateFormatted) %>%
        summarise(consumed = sum(Type=='Cheated' | Type=='On time' | Type=='Behaviour'))
      sum(df$consumed)/nrow(df)
    })
    
    avgCigWeekday <- reactive({
      df <- logs.filtered()
      df <- df %>%
        group_by(dateFormatted) %>%
        filter(Weekday=='Monday' | Weekday=='Tuesday' | Weekday=='Wednesday' | Weekday=='Thursday' | Weekday=='Friday') %>%
        summarise(consumed = sum(Type=='Cheated' | Type=='On time' | Type=='Behaviour'))
      sum(df$consumed)/nrow(df)
    })
    
    avgCigWeekend <- reactive({
      df <- logs.filtered()
      df <- df %>%
        group_by(dateFormatted) %>%
        filter(Weekday=='Saturday' | Weekday=='Sunday') %>%
        summarise(consumed = sum(Type=='Cheated' | Type=='On time' | Type=='Behaviour'))
      sum(df$consumed)/nrow(df)
    })

    totalConsumption <- reactive({
      logs <- logs.consumptionWeekday()
      df <- count(logs, Weekday)
      if(nrow(df) == 0){
        0
      }else{
        sum(df[2])
      }
    })

    weekdayConsumption <- reactive({
      logs <- logs.consumptionWeekday()
      df <- count(logs, Weekday)
      consumption_weekdays.data <- subset(df, Weekday == "Monday" | Weekday == "Tuesday" | Weekday == "Wednesday" | Weekday == "Thursday" | Weekday == "Friday", select = c("Weekday", "n"))
      if(nrow(consumption_weekdays.data) == 0){
        0
      }else{
        sum(consumption_weekdays.data[2])
      }
    })

    weekendConsumption <- reactive({
      logs <- logs.consumptionWeekday()
      df <- count(logs, Weekday)
      consumption_weekends.data <- subset(df, Weekday == "Saturday" | Weekday == "Sunday", select = c("Weekday", "n"))
      if(nrow(consumption_weekends.data) == 0){
        0
      }else{
        sum(consumption_weekends.data[2])
      }
    })

    lastSevenDays <- reactive({
      user <- logs %>% filter(User==input$user)
      consumption <- user %>% filter(Type=='Cheated' | Type=='On Time')
      seven_days <- consumption %>% filter(dateFormatted > today() - 7)
      count(seven_days)$n
    })

    # weekProgress(week, user) <- reactive({
    #   if(week == 1 | week == 2){
    #     behavior <- user %>% filter(Type=='Behaviour')
    #     behavior <- count(behavior)
    #     consumption <- user %>% filter(userDate == week, Type=='Cheated' | Type=='On Time')
    #     consumption <- count(consumption)
    #     (behavior - consumption) / behavior
    #   }else{
    #     consumptions = c()
    #     for(i in 1:3){
    #       consumption <- user %>% filter(userDate == (week - i), Type=='Cheated' | Type=='On Time')
    #       consumption <- count(consumption)
    #       consumptions[i] <- consumption$n
    #     }
    #     past_weeks <- mean(consumptions)
    #     consumption <- user %>% filter(userDate == week, Type=='Cheated' | Type=='On Time')
    #     (past_weeks - consumption) / past_weeks
    #   }
    # })

    # userProgress() <- reactive({
    #   user <- logs %>% filter(User==input$user)
    #   dates <- user$dateFormatted
    #   week <- as.numeric(dates-dates[1]) %/% 7
    #   user$userDate <- week
    #   progress <- c()

    # })
    
    peakTimeSlot <- reactive({
      df <- logs.filterSmokedAndUser()
      df$h <- with(df, substr(df$OnlyTime, 1, 2))

      df2 <- data.frame(df)
      df2$h <- as.numeric(as.character(df2$h))
      df2$slot <- with(df2, h-h%%2)
      
      df3 <- df2 %>% group_by(slot) %>% summarise(f = n())
      df4 <- df3[order(-df3$f),]
      df4$slot[1]
    })
    
    avgEngagement <- reactive({
      user <- logs %>% filter(User==input$user)
      dates <- user$dateFormatted
      week <- as.numeric(dates-dates[1]) %/% 7
      user$userDate <- week
      weeks <- unique(week)
      # Data frame to plot the engagement of each week
      df <- data.frame(Week=integer(), Engagement=double())
      for(value in weeks){
        auto_skipped <- user %>% filter(userDate==value, Type=="Auto skipped")
        auto_skipped <- count(auto_skipped)$n
        other_types <- user %>% filter(userDate==value, Type=="Auto skipped" | Type=="Skipped" | Type=="On Time" | Type=="Snoozed")
        other_types <- count(other_types)$n
        engagement <- 1 - (auto_skipped/other_types)
        engagement <- engagement * 100
        temp <- data.frame(value, engagement)
        names(temp) <- c("Week", "Engagement")
        df <- rbind(df, temp)
      }
      df[is.na(df)] <- 0
      df <- df %>%
        group_by(Week) %>%
        filter(Week>0) %>%
        summarise(eng = sum(Engagement))
      sum(df$eng)/nrow(df)
    })
  
  #cig saved AU
  logs.filtered.each <- function(name){
    x <- logs %>% filter(User==name)
  }
  
  countRow <- function(user) {
    count(logs.filter(user))$n[1]
  }
  
  cigSavedEach <- function(user) {
    df <- logs.filtered.each(user) #filterd logs df
    userStartDate <- df$dateFormatted[1] #date
    userBehaviorWeek <- with(df, df[(dateFormatted <= userStartDate + 7), ]) #first week df
    userOtherWeek <- with(df, df[(dateFormatted > userStartDate + 7), ]) #other weeks df
    df2 <- userBehaviorWeek %>% filter(Type=='Behaviour')
    data <- count(df2)
    userBehaviorWeek.consumed <- data$n[1] #single value
    userOtherWeek.consumed <- userOtherWeek %>%
      group_by(week) %>%
      summarise(all = n(),consumed = sum(Type=='Cheated' | Type=='On time')) #df with count column
    df3 <- userOtherWeek.consumed
    df3$saved <- with(df3, userBehaviorWeek.consumed - df3$consumed)
    sum(df3$saved)
  }
  
  userList <- data.frame(unique(logs$User))
  colnames(userList) <- "user"
  userList$cigSaved <- sapply(userList$user, cigSavedEach) #apply function to new row
  cigSavedAll <- sum(userList$cigSaved)
  
  avgCigSaved <- cigSavedAll/nrow(userList)
  
  #AU/info output
  output$cigSavedAll <- renderText({
    cigSavedAll
  })
  
  output$moneySavedAll <- renderText({
    paste(cigSavedAll,"$")
  })
  
  output$avgCigSaved <- renderText({
    avgCigSaved
  })
  
  output$avgMoneySaved <- renderText({
    paste(avgCigSaved,"$")
  })
  
  #SU/week slider range
  observe({
    df <-logs.filtered()
    suWeek <- unique(df$week)
    suFirstWeek <- suWeek[1]
    suLastWeek <- tail(suWeek, n = 1)
    updateSliderInput(session, "cigWeekSlotSlider", min = suFirstWeek, max = suLastWeek, value = suFirstWeek)
    updateSliderInput(session, "cigWeekSlider", min = suFirstWeek, max = suLastWeek, value = suFirstWeek)
    updateSliderInput(session, "modeWeekSlider", min = suFirstWeek, max = suLastWeek, value = suFirstWeek)
  })
    
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

    output$meanAndStd <- renderPlotly({
      user <- logs %>% filter(User==input$user)
      consumption <- user %>% filter(Type=="Cheated" | Type== "On Time" | Type=="Behaviour")
      consumption_weekdays <- count(consumption, Weekday, Type)
      weekdays <- unique(consumption_weekdays$Weekday)
      # Data frame to plot the mean and std for each weekday
      df <- data.frame(Weekday=character(), Mean=double(), Std=double())
      for(value in weekdays){
        day <- consumption_weekdays %>% filter(Weekday==value)
        rows <- nrow(day)
        if(rows == 1){
          dummy <- data.frame(value, "Dummy1", 0)
          names(dummy) <- c("Weekday", "Type", "n")
          day <- rbind(day, dummy)

          dummy <- data.frame(value, "Dummy2", 0)
          names(dummy) <- c("Weekday", "Type", "n")
          day <- rbind(day, dummy)
        } else if (rows == 2){
          dummy <- data.frame(value, "Dummy", 0)
          names(dummy) <- c("Weekday", "Type", "n")
          day <- rbind(day, dummy)
        }
        mean <- mean(day$n)
        std <- sd(day$n)
        temp <- data.frame(value, mean, std)
        names(temp) <- c("Weekday", "Mean", "Std")
        df <- rbind(df, temp)
      }
      p <- plot_ly(df, x = ~Weekday, y = ~Mean, type = 'bar', error_y = ~list(array = Std, color = '#000000'))
    })

    output$engagementPerDay <- renderPlotly({
      user <- logs %>% filter(User==input$user)
      user_dates <- unique(user$dateFormatted)
      # Data frame to plot the engagement of each day
      df <- data.frame(Day=integer(), Engagement=double())
      cont <- 1
      for(value in user_dates){
        auto_skipped <- user %>% filter(dateFormatted==value, Type=="Auto skipped")
        auto_skipped <- count(auto_skipped)$n
        other_types <- user %>% filter(dateFormatted==value, Type=="Auto skipped" | Type=="Skipped" | Type=="On Time" | Type=="Snoozed")
        other_types <- count(other_types)$n
        engagement <- 1 - (auto_skipped/other_types)
        engagement <- engagement * 100
        temp <- data.frame(cont, engagement)
        names(temp) <- c("Day", "Engagement")
        df <- rbind(df, temp)
        cont <- cont + 1
      }
      p <- plot_ly(df, x = ~Day, y = ~Engagement, type = 'scatter', mode = 'lines')
    })

    output$engagementPerWeek <- renderPlotly({
      user <- logs %>% filter(User==input$user)
      dates <- user$dateFormatted
      week <- as.numeric(dates-dates[1]) %/% 7
      user$userDate <- week
      weeks <- unique(week)
      # Data frame to plot the engagement of each week
      df <- data.frame(Week=integer(), Engagement=double())
      for(value in weeks){
        auto_skipped <- user %>% filter(userDate==value, Type=="Auto skipped")
        auto_skipped <- count(auto_skipped)$n
        other_types <- user %>% filter(userDate==value, Type=="Auto skipped" | Type=="Skipped" | Type=="On Time" | Type=="Snoozed")
        other_types <- count(other_types)$n
        engagement <- 1 - (auto_skipped/other_types)
        engagement <- engagement * 100
        temp <- data.frame(value, engagement)
        names(temp) <- c("Week", "Engagement")
        df <- rbind(df, temp)
      }
      p <- plot_ly(df, x = ~Week, y = ~Engagement, type = 'scatter', mode = 'lines')
    })
    
    #single user valueBoxOutput content
    genderIcon <- function(gender){
      if(gender == "Male"){
        "mars"
      }else if(gender == "Female"){
        "venus"
      }else{
        "question"
      }
    }
    
    genderColor <- function(gender){
      if(gender == "Male"){
        "aqua"
      }else if(gender == "Female"){
        "fuchsia"
      }else{
        "purple"
      }
    }
    
    output$userGender <- renderInfoBox({
      data <- surveydata.filterUser()
      if((input$user %in% surveydata$Name) == FALSE){
        g <- "No info"
      }else{
        g <- data$Gender 
      }
      infoBox("Gender", 
              g, 
              icon = icon(genderIcon(g), lib = "font-awesome"),
              color = genderColor(g))
    })
    
    output$userAge <- renderInfoBox({
      data <- surveydata.filterUser()
      infoBox("Age", 
              if((input$user %in% surveydata$Name) == FALSE){
                "No info"
              }else{
                data$Age
              }, 
              icon = icon("birthday-cake", lib = "font-awesome"),
              color = "green")
    })
    
    output$userMarriage <- renderInfoBox({
      data <- surveydata.filterUser()
      infoBox("Family Status", 
              if((input$user %in% surveydata$Name) == FALSE){
                "No info"
              }else{
                data$`Family status`
              }, 
              icon = icon("heart", lib = "font-awesome"),
              color = "maroon")
    })
    
    #SU/info output
    ageCategory <- function(a){
      if(a < 30){
        "Young"
      }else if(a >= 30 && a < 50){
        "Middle"
      }else{
        "Old"
      }
    }
    
    output$ageCat <- renderText({
      data <- surveydata.filterUser()
      if((input$user %in% surveydata$Name) == FALSE){
        "-"
      }else{
        ageCategory(data$Age) 
      }
    })
    
    output$cigSaved <- renderText({
      cigSaved()
    })
    
    output$moneySaved <- renderText({
      paste(cigSaved(),"$")
    })
    
    output$avgCigPerDay <- renderText({
      avgCigPerDay()
    })
    
    output$avgCigWeekday <- renderText({
      avgCigWeekday()
    })
    
    output$avgCigWeekend <- renderText({
      avgCigWeekend()
    })

    output$totalConsumption <- renderText({
      totalConsumption()
    })

    output$weekendConsumption <- renderText({
      weekendConsumption()
    })

    output$weekdayConsumption <- renderText({
      weekdayConsumption()
    })

    output$lastSevenDays <- renderText({
      lastSevenDays()
    })
    
    output$peakTimeSlot <- renderText({
      a <- as.numeric(peakTimeSlot())
      b <- a+2
      paste(a, ":00 - ", b, ":00", sep = "")
    })
    
    output$avgEngagement <- renderText({
      avgEngagement()
    })
    
    #Total number of each mode
    output$totalByType <- renderPlotly({
      logs <- logs.filtered()
      plot_ly(logs, labels = logs$Type, type = "pie")
    })

    #Total number of each mode all users
    output$totalByTypeAll <- renderPlotly({
      plot_ly(logs, labels = logs$Type, type = "pie")
    })

    #cigarette consumption per weekday
    output$freqWeekday <- renderPlotly({
      logs <- logs.filterSmokedAndUser()
      df <- count(logs, Weekday)
      #cat(file=stderr(),df)
      df$Weekday <- factor(df$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      plot_ly(data = df, x = df$Weekday, y = df$n, type = 'bar')
    })

    #cigarette consumption per weekday all users
    output$freqWeekdayAll <- renderPlotly({
      logs <- logs.Smoked()
      df <- count(logs, Weekday)
      #cat(file=stderr(),df)
      df$Weekday <- factor(df$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      plot_ly(data = df, x = df$Weekday, y = df$n, type = 'bar')
    })

    #frequency graph by time
    output$freqTime <- renderPlotly({
      logs <- logs.filterModeAndUser()
      df <- count(logs, Date)
      df$newdate <- strptime(as.character(df$Date), "%d/%m/%Y")
      df$plotlydate <- format(df$newdate, "%Y-%m-%d")
      df <- df[order(as.Date(df$plotlydate, format = "%Y-%m-%d")),]
      plot_ly(df, x = df$plotlydate, y = df$n, type = 'scatter', mode = 'lines')
    })
    
    # >>> SU/week output <<< #
    #Cigarettes per weekday per time slots
    logs.filterCigWeekSlot <- reactive({
      df <- logs.filtered()
      x <- df %>% filter(week==input$cigWeekSlotSlider, Type=='Behaviour' | Type=='Cheated' | Type=='On time')
    })
    
    output$cigWeekSlot <- renderPlotly({
      df <- logs.filterCigWeekSlot()
      df$h <- with(df, substr(df$OnlyTime, 1, 2))
      #df2 <- ddply(df, .(df$Weekday, df$h), nrow)
      df2 <- df %>% group_by(Weekday, h) %>% summarise(Freq = n())
      df3 <- data.frame(df2)
      df3$h <- as.numeric(as.character(df3$h))
      df3$slot <- with(df3, h-h%%2)
      df3$Weekday <- factor(df3$Weekday, daysofweek)
      df4 <- df3 %>% complete(Weekday = factor(df$Weekday, levels = daysofweek), slot = timeSlot, fill = list(Freq = 0))

      #ref <- expand.grid(daysofweek, timeSlot)
      #names(ref) <- c("Weekday","h")
      #ref$Freq <- 0
      #m <- spread(df2, h, Freq, fill = 0)
      
      plot_ly(df4, x=df4$slot, y=df4$Weekday, z=df4$Freq, type = "heatmap") %>% 
        layout(xaxis = list(dtick = 2, title = "Time Slot"))
    })
    
    #Comparison of cigarettes consumption between weeks
    output$cigAllWeek <- renderPlotly({
      df <- logs.filterSmokedAndUser()
      df2 <- df %>% group_by(week, Weekday) %>% summarise(Freq = n())
      df3 <- df2 %>% group_by(week) %>% summarise(Freq = mean(Freq))
      plot_ly(df3, x=df3$week, y=df3$Freq, type = "bar") %>%
        add_lines(y = ~fitted(loess(df3$Freq ~ df3$week, span=1)),
                  name = "Loess Smoother", showlegend = FALSE) %>% 
        layout(xaxis = list(title = "Week", dtick = 1),
               yaxis = list(title = "Mean cigarettes per weekday"))
    })
    
    #Mode usage per week
    logs.filterModeWeek <- reactive({
      df <- logs.filtered()
      x <- df %>% filter(week==input$modeWeekSlider)
    })
    
    output$modeWeek <- renderPlotly({
      logs <- logs.filterModeWeek()
      plot_ly(logs, labels = logs$Type, type = "pie")
    })
    
    #Cigarettes consumption per weekday
    logs.filterCigWeek <- reactive({
      df <- logs.filtered()
      x <- df %>% filter(week==input$cigWeekSlider, Type=='Behaviour' | Type=='Cheated' | Type=='On time')
    })
    
    output$cigWeek <- renderPlotly({
      df <- logs.filterCigWeek()
      df2 <- df %>% group_by(Weekday) %>% summarise(Freq = n())
      df3 <- data.frame(df2)
      df3$Weekday <- factor(df3$Weekday, daysofweek)
      df4 <- df3 %>% complete(Weekday = factor(df$Weekday, levels = daysofweek), fill = list(Freq = 0))
      plot_ly(df4, x=df4$Weekday, y=df4$Freq, type = "bar")
    })
    
    # >>> SU/all output <<< #
    #usage by time
    output$allUsage <- renderPlotly({
      logs <- logs.filtered()
      df <- logs %>% group_by(dateFormatted) %>% summarise(f = n()) %>% ungroup(df)
      plot_ly(df, x = ~dateFormatted, y = df$f, type = 'scatter', mode = 'lines') %>%
        layout(xaxis = list(title = 'Date'),
               yaxis = list(title = 'Number of uses'))
    })
    
    #mode usage by time
    output$allModeUsage <- renderPlotly({
      logs <- logs.filtered()
      df <- logs %>% group_by(dateFormatted, Type) %>% summarise(f = n()) %>% ungroup(df)
      range <- seq(userStartDate(), userEndDate(), "days")
      df <- df %>% complete(dateFormatted =  range, nesting(Type), fill = list(f = 0))
      plot_ly(df, x = ~dateFormatted, y = df$f, type = 'scatter', mode = 'lines', split = df$Type) %>%
        layout(xaxis = list(title = 'Date'),
               yaxis = list(title = 'Number of uses'))
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

#Shiny dashboard: Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.