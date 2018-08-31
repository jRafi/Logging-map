
# Dependencies
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(rgdal)
municipalities <- readRDS("municipalities.rds")
DataDate <- read.table("date.txt", stringsAsFactors = F)


shinyApp(
        ui <- dashboardPage(
                skin = "green",
                dashboardHeader(
                        title = "Skogsavverkning"),
                dashboardSidebar(
                        paste("Data senast hämtad: ", DataDate, "", sep = ""),
                        selectInput("county",
                                    label = h3("1. Välj län"),
                                    choices = names(municipalities),
                                    selected = ("")
                        ),
                        conditionalPanel(condition = "input.county != ' '",
                                         uiOutput("municipalityOutput")
                        ),
                        conditionalPanel(condition = "input.munies != ' '",
                                         uiOutput("yearsSlider"))
                        ),
                dashboardBody( mainPanel(
                        leafletOutput(outputId = "map")
                ))
        ),
        
        
        server = function(input, output) {
                
                # Conditional UI-panel 1
                output$municipalityOutput <- renderUI({
                        selectInput("munies",
                                    label = h3("2. Välj kommun"),
                                    municipalities[[input$county]],
                                    selected = "")
                })
                
                # Conditional UI-panel 2
                output$yearsSlider <- renderUI({
                        sliderInput("years",
                                    label = h3("3. Välj tidsperiod"),
                                    min = 1998, 
                                    #max = as.numeric(format(Sys.Date(), "%Y")),
                                    max = 2018,
                                    value = c(2017,2018),
                                    sep = "",
                                    width = "90%",
                                    ticks = F
                        )
                })
                
                # Select data
                dataInput <- reactive({
                        dataInput <- readOGR(dsn = paste("Git Local/Skogsavverkning/data/Municipalities/", input$muni, sep = ""),
                                             layer = input$list)
                        dataInput@data <- mutate(dataInput@data, year = as.numeric(levels(year))[dataInput@data$year])
                        dataInput
                })
                
                
                # Select years
                dataSelect <- reactive({
                        dataSel <- dataInput()
                        dataSel <- dataSel[dataSel$year >= input$years[1] & dataSel$year <= input$years[2],]
                        dataSel
                })
                
                
                # Leaflet map
                output$map <- renderLeaflet({
                        leaflet() %>% 
                                addTiles() %>%
                                fitBounds(10, 69, 24, 55)
                })
                
                observe({
                        leafletProxy("map", data = dataSelect()) %>%
                                clearShapes() %>%
                                addPolygons()
                })


                
        },
        options = list(height = 600)
)


