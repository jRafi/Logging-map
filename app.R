
tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

# Dependencies
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(rgdal)
municipalities <- readRDS("municipalities.rds")
dataDate <- read.table("date.txt", stringsAsFactors = F)


shinyApp(
        ui <- dashboardPage(
                dashboardHeader(
                        title = "Skogskoll.se"),
                dashboardSidebar(
                        selectInput("county",
                                    label = h3("1. Välj län"),
                                    choices = names(municipalities),
                                    selected = ""),
                        uiOutput("ui_muni"),
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
                        if(dir.exists(paste("data/Municipalities/", input$munies, sep = ""))) {
                                readOGR(dsn = paste("data/Municipalities/", input$munies, sep = ""),
                                        layer = input$munies, verbose = F)
                        }
                })
                
                dataSel <- reactive({
                        if(dir.exists(paste("data/Municipalities/", input$munies, sep = ""))) {
                                dataSel <- dataInput()
                                dataSel@data <- mutate(dataSel@data, year = as.numeric(levels(year))[dataSel@data$year])
                                dataSel <- dataSel[dataSel$year >= input$years[1] & dataSel$year <= input$years[2],]
                                dataSel
                        }
                })
                
                popups <- reactive({
                        if(dir.exists(paste("data/Municipalities/", input$munies, sep = ""))) {
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
                        if(dir.exists(paste("data/Municipalities/", input$munies, sep = ""))) {
                                leafletProxy("map", data = dataSel()) %>%
                                        clearShapes() %>%
                                        addPolygons(layerId = ~id)
                        }
                })
                
                #output$progressBox <- renderPrint({
                #        filter(popups(), id == input$map_shape_click)
                #})
                
                
        },
        options = list(height = 600)
)


