library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

recommendation <- read.csv('recommendation.csv', stringsAsFactors = F, header = T)
surveydata <- readxl::read_xlsx('surveydataece (1).xlsx')
logs <- read.csv('logs (1).csv', sep = ";")

saveRDS(logs, "./logs.rds")
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
        title = "Revenue per account",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("revenuebyPrd", height = "300px")
    ),
    box(
        title = "Revenue per product",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("revenuebyRegion", height = "300px")
    )
)

frow21 <- fluidRow(
  box(
    title = "Total number of each mode",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotlyOutput("totalByType")
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
    tabItem(tabName = "singleUser", frow21),
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
    
    # valueBoxOutput content
    output$value1 <- renderValueBox({
        valueBox(
            formatC(sales.account$value, format = "d", big.mark = ","),
            paste("Top Account:", sales.account$Account),
            icon = icon("stats", lib = "glyphicon"),
            color = "purple")
    })

    output$value2 <- renderValueBox({
        valueBox(
            formatC(total.revenue, format = "d", big.mark = ","),
            "Total Expected Revenue",
            icon = icon("gbp", lib = "glyphicon"),
            color = "green")
    })

    output$value3 <- renderValueBox({
        valueBox(
            formatC(prof.prod$value, format = "d", big.mark = ","),
            paste("Top Product:", prof.prod$Product),
            icon = icon("menu-hamburger", lib = "glyphicon"),
            color = "yellow")
    })

    # plotOutput content
    output$revenuebyPrd <- renderPlot({
        ggplot(data = recommendation,
               aes(x = Product, y = Revenue, fill = factor(Region))) + 
               geom_bar(position = "dodge", stat = "identity") +
               ylab("Revenue (in Euros)") +
               xlab("Product") +
               theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "bold")) +
               ggtitle("Revenue by Product") + labs(fill = "Region")
    })

    output$revenuebyRegion <- renderPlot({
        ggplot(data = recommendation,
               aes(x = Account, y = Revenue, fill = factor(Region))) + 
               geom_bar(position = "dodge", stat = "identity") +
               ylab("Revenue (in Euros)") +
               xlab("Account") +
               theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "bold")) +
               ggtitle("Revenue by Region") + labs(fill = "Region")
    })
    
    output$totalByType <- renderPlotly({
      logs <- logs.filtered()
      plot_ly(x = logs$Type, type = "histogram")
    })
    
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