
# Dependencies
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(rgdal)
municipalities <- readRDS("Git Local/Skogsavverkning/municipalities.rds")
dataDate <- read.table("Git Local/Skogsavverkning/date.txt", stringsAsFactors = F)


shinyApp(
        ui <- dashboardPage(
                dashboardHeader(
                        title = "Skogsavverkning"),
                dashboardSidebar(
                        selectInput("county",
                                    label = h3("1. Välj län"),
                                    choices = names(municipalities),
                                    selected = ""),
                        uiOutput("ui_muni"),
                        #selectInput("list",
                        #            label = h3("2. Välj kommun"),
                        #            c(municipalities[[input$county()]]),
                        #            selected = "---"),
                        #selectInput("list", "Lista", c("Gotland", "---"), selected = "Gotland"),
                        sliderInput("years",
                                    label = h3("3. Välj tidsperiod"),
                                    min = 1998, 
                                    max = as.numeric(format(Sys.Date(), "%Y")),
                                    value = c(2017,2018),
                                    sep = "",
                                    width = "90%",
                                    ticks = F
                        )
                ),
                dashboardBody(mainPanel(
                        fluidRow(
                                valueBoxOutput("progressBox"),
                                verbatimTextOutput("munies"),
                                leafletOutput(outputId = "map")
                        )))),
        
        server = function(input, output) {
                
                output$ui_muni <- renderUI({
                        selectInput("munies",
                                    label = h3("2. Välj kommun"),
                                    choices = municipalities[[input$county]],
                                    selected = "")
                })
                
                dataInput <- eventReactive(input$munies, {
                        if(dir.exists(paste("Git Local/Skogsavverkning/data/Municipalities/", input$munies, sep = ""))) {
                                readOGR(dsn = paste("Git Local/Skogsavverkning/data/Municipalities/", input$munies, sep = ""),
                                        layer = input$munies)
                        }
                })
                
                
                #, verbose = F)
                #dataInput@data <<- mutate(dataInput@data, year = as.numeric(levels(year))[dataInput@data$year])
                #dataInput
                
                
                dataSel <- reactive({
                        if(dir.exists(paste("Git Local/Skogsavverkning/data/Municipalities/", input$munies, sep = ""))) {
                                dataSel <- dataInput()
                                dataSel@data <- mutate(dataSel@data, year = as.numeric(levels(year))[dataSel@data$year])
                                dataSel <- dataSel[dataSel$year >= input$years[1] & dataSel$year <= input$years[2],]
                                dataSel
                        }
                })
                
                popups <- reactive({
                        if(dir.exists(paste("Git Local/Skogsavverkning/data/Municipalities/", input$munies, sep = ""))) {
                                popups <- dataInput()
                                popups <- popups@data
                        }
                })
                
                # Leaflet map
                output$map <- renderLeaflet({
                        leaflet() %>% 
                                addTiles() %>%
                                fitBounds(10, 69, 24, 55)
                })
                
                observe({
                        if(dir.exists(paste("Git Local/Skogsavverkning/data/Municipalities/", input$munies, sep = ""))) {
                                leafletProxy("map", data = dataSel()) %>%
                                        clearShapes() %>%
                                        addPolygons(layerId = ~id)
                        }
                })
                
                output$progressBox <- renderPrint({
                        filter(popups(), id == input$map_shape_click)
                })
                
                
        },
        options = list(height = 600)
)


