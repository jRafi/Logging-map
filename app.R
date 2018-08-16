
# Dependencies
library(dplyr)
library(leaflet)
library(shiny)
library(rgdal)
municipalities <- readRDS("municipalities.rds")


shinyApp(
        ui = fluidPage(
                titlePanel("Avverkningsanmälningar"),
                sidebarLayout(
                        sidebarPanel(
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
                        mainPanel(
                                leafletOutput(outputId = "map")
                        )
                )
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
                
                # Reactive module 2
                dataSelected <- reactive({
                        dataInput <- readOGR(dsn = paste("data/Municipalities/", input$munies, sep = ""),
                                             layer = input$munies)#, verbose = F)
                        #dataInput$year <- as.numeric(dataInput$year) #levels(dataInput$year))[dataInput$year]
                        dataInput@data <- mutate(dataInput@data, year = as.numeric(levels(year))[dataInput@data$year])
                        dataInput
                })
                
                
                # Reactive module 3
                dataSel3 <- reactive({
                        dataYears <- dataSelected()
                        dataYears <- dataYears[dataYears$year >= input$years[1] & dataYears$year <= input$years[2],]
                        #dataYears@data <- filter(dataYears@data, year >= input$years[1])
                        #dataYears@data <- mutate(dataYears@data, year = as.numeric(levels(year))[dataYears@data$year])
                        dataYears
                })
                
                
                # Leaflet map
                output$map <- renderLeaflet({
                        leaflet() %>% 
                                addProviderTiles("CartoDB.Positron") %>%
                                fitBounds(10, 69, 24, 55) %>% 
                                addPolygons(data = dataSel3())
                        
                })
                
        },
        options = list(height = 600)
)


