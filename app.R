
# Load dependecies
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(sf)

# Read list of municipalities
municipalities <- readRDS("municipalities.rds")

# Check which date the data is from
date <- read.table("date.txt", stringsAsFactors = F)

# For use with some lazy coding
senaste <- readRDS("Senaste_x.rds")

shinyApp(
        ui <- dashboardPage(
                dashboardHeader(
                        title = "Skogskoll.se"),
                dashboardSidebar(
                        selectInput("county",
                                    label = h4("1. Välj län"),
                                    choices = names(municipalities),
                                    selected = "Inget"),
                        uiOutput("ui_muni"),
                        uiOutput("ui_years")),
                dashboardBody(
                        fluidRow(
                                tags$head(
                                        tags$style(HTML("
                                                .main-header .logo {
                                                text-align: justify;
                                                font-size: 30px;
                                                font-style: bold;
                                                color: #48ca3b;
                                                }
                                                
                                                "))),
                                box(solidHeader = TRUE,
                                    width = 12,
                                    leafletOutput(outputId = "map")
                                ),
                                box(solidHeader = TRUE,
                                    width = 7,
                                    checkboxInput(inputId = "anmBox",
                                                  label = "Visa avverkningsanmälningar inom vald tidsperiod",
                                                  value = T),
                                    checkboxInput(inputId = "utfBox",
                                                  label = "Visa utförda avverkningar inom vald tidsperiod"),
                                    checkboxInput(inputId = "bioBox",
                                                  label = "Visa biotopskydd")
                                ))
                ) 
        ),
        server <- function(input, output, server) {
                
                ### UI
                output$ui_muni <- renderUI({
                        selectInput("munies",
                                    label = h4("2. Välj filter"),
                                    choices = municipalities[[input$county]],
                                    selected = "Inget")
                })
                
                output$ui_years <- renderUI({
                        if(!input$munies %in% senaste){
                                sliderInput("years",
                                            label = h4("3. Välj tidsperiod"),
                                            min = 1998, 
                                            max = as.numeric(format(Sys.Date(), "%Y")),
                                            value = c(2017,2018),
                                            sep = "",
                                            width = "90%",
                                            ticks = F)
                        }
                        else{
                                
                        }
                })
                
                ### Data
                anmData <- eventReactive(input$munies, {
                        if(dir.exists(paste("data/anm/", input$munies, sep = ""))) {
                                read_sf(paste("data/anm/", input$munies, sep = ""))
                        }
                        else{NULL}
                })
                
                utfData <- eventReactive(input$munies, {
                        if(dir.exists(paste("data/utf/", input$munies, sep = ""))) {
                                read_sf(paste("data/utf/", input$munies, sep = ""))
                        }
                })
                
                bioData <- eventReactive(input$munies, {
                        if(dir.exists(paste("data/bio/", input$munies, sep = ""))) {
                                read_sf(paste("data/bio/", input$munies, sep = ""))
                        }
                })
                
                ### Leaflet
                output$map <- renderLeaflet({
                        leaflet() %>% 
                                addProviderTiles("CartoDB.Positron") %>%
                                fitBounds(10, 69, 24, 55)
                })
                
                
                observe({
                        selectedYears <- c(input$years[1], input$years[2])
                        leafletProxy("map") %>%
                                clearGroup("anm") %>%
                                clearGroup("utf")
                        if(input$anmBox == TRUE) {
                                if(dir.exists(paste("data/anm/", input$munies, sep = ""))) {
                                        filteredAnmData <- anmData()
                                        filteredAnmData <- filteredAnmData[filteredAnmData$Arendear >= selectedYears[1] & filteredAnmData$Arendear <= selectedYears[2],]
                                        leafletProxy("map", data = filteredAnmData) %>%
                                                addPolygons(layerId = ~OBJECTID,
                                                            color = "forestgreen",
                                                            group = "anm",
                                                            popup = as.character(tagList(
                                                                    tags$strong("Inkomstdatum:"), filteredAnmData$Inkomdatum, tags$br(),
                                                                    tags$strong("Skogstyp:"), filteredAnmData$Skogstyp, tags$br(),
                                                                    tags$strong("Avverkningstyp:"), filteredAnmData$Avverktyp, tags$br(),
                                                                    tags$strong("Anmäld HA:"), filteredAnmData$Anmaldha, tags$br(),
                                                                    tags$strong("Skogsodlha:"), filteredAnmData$Skogsodlha, tags$br(),
                                                                    tags$strong("Natforha:"), filteredAnmData$Natforha, tags$br(),
                                                                    tags$strong("Avvha:"), filteredAnmData$Avvha, tags$br()
                                                            )
                                                            ))
                                }
                        }
                        else(leafletProxy("map") %>%
                                     clearGroup("anm"))
                        
                        if(input$utfBox == TRUE) {
                                if(dir.exists(paste("data/utf/", input$munies, sep = ""))) {
                                        filteredUtfData <- utfData()
                                        filteredUtfData <- filteredUtfData[filteredUtfData$Arendear >= selectedYears[1] & filteredUtfData$Arendear <= selectedYears[2],]
                                        leafletProxy("map", data = filteredUtfData) %>%
                                                clearGroup("utf") %>%
                                                addPolygons(layerId = ~OBJECTID,
                                                            color = "darkorange",
                                                            group = "utf",
                                                            popup = as.character(tagList(
                                                                    tags$strong("Avverkningsdatum:"), filteredUtfData$Avvdatum, tags$br(),
                                                                    tags$strong("Skogstyp:"), filteredUtfData$Skogstyp, tags$br(),
                                                                    tags$strong("Avverkningstyp:"), filteredUtfData$Avverktyp, tags$br(),
                                                                    tags$strong("Anmäld HA:"), filteredUtfData$AnmaldHa, tags$br(),
                                                                    tags$strong("Skogsodlha:"), filteredUtfData$SkogsodlHa, tags$br(),
                                                                    tags$strong("Natforha:"), filteredUtfData$Natforha, tags$br(),
                                                                    tags$strong("Arealha:"), filteredUtfData$Arealha, tags$br()
                                                            )
                                                            ))
                                }
                        }
                        else(leafletProxy("map") %>%
                                     clearGroup("utf"))
                })
                
                observe({
                        bioSelected <- bioData()
                        
                        if(input$bioBox == TRUE) {
                                if(dir.exists(paste("data/bio/", input$munies, sep = ""))) {
                                        bioSelected <- bioData()
                                        leafletProxy("map", data = bioSelected) %>%
                                                clearGroup("bio") %>%
                                                addPolygons(layerId = ~OBJECTID,
                                                            color = "purple",
                                                            group = "bio",
                                                            popup = as.character(tagList(
                                                                    tags$strong("Biotopskydd"), tags$br(),
                                                                    tags$strong("Biotopkategori:"), bioSelected$Biotyp, tags$br(),
                                                                    tags$strong("Skogstyp:"), bioSelected$Naturtyp, tags$br(),
                                                                    tags$strong("Beslutsdatum:"), bioSelected$Datbeslut, tags$br(),
                                                                    tags$strong("Produktiv skogsmark (ha):"), bioSelected$Areaprod, tags$br(),
                                                                    tags$strong("Total areal (ha):"), bioSelected$Areato, tags$br(),
                                                                    tags$strong("Standort:"), bioSelected$Standort, tags$br(),
                                                                    tags$strong("Url:"), tags$a(href=bioSelected$Url, bioSelected$Url)
                                                            )
                                                            ))
                                }
                        }
                        else(leafletProxy("map") %>%
                                     clearGroup("bio"))
                })
                
        }
)

