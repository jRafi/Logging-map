
# Load dependecies
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(sf)

# Read list of municipalities
municipalities <- readRDS("municipalities3.rds")

# Check which date the data is from
date <- read.table("date.txt", stringsAsFactors = F)

# For use with some lazy coding
senaste <- readRDS("senaste.rds")

shinyApp(
        ui <- dashboardPage(
                dashboardHeader(
                        title = "Skogskoll.se"),
                dashboardSidebar(disable = TRUE
                ),
                dashboardBody(
                        tags$head(includeScript("gtag.js"),
                                  tags$style(HTML("
                                                .main-header .logo {
                                                  text-align: justify;
                                                  font-size: 30px;
                                                  font-style: bold;
                                                  color: #48ca3b;
                                                  }
                                                  "))),
                        fluidRow(
                                column(width = 3,
                                       tabBox(width = NULL,
                                              height = "200px",
                                              title = "",
                                              id = "tabset1",
                                              tabPanel("Senast inkommet",
                                                       value = "tab1",
                                                       #actionButton("goButton", "Go!"),
                                                       selectizeInput("latest",
                                                                      label = "",
                                                                      choices = NULL,
                                                                      selected = NULL,
                                                                      options = list(placeholder = 'Välj län'))),
                                              tabPanel("Historisk data",
                                                       value = "tab2",
                                                       selectizeInput("munies",
                                                                      label = "",
                                                                      choices = NULL,
                                                                      selected = NULL,
                                                                      options = list(placeholder = 'Sök kommun...')),
                                                       uiOutput("ui_muni"),
                                                       sliderInput("years",
                                                                   label = "",
                                                                   min = 1998, 
                                                                   max = as.numeric(format(Sys.Date(), "%Y")),
                                                                   value = c(as.numeric(format(Sys.Date(), "%Y"))-1,
                                                                             as.numeric(format(Sys.Date(), "%Y"))),
                                                                   sep = "",
                                                                   width = "90%",
                                                                   ticks = F))
                                              
                                              #uiOutput("ui_muni"),
                                              #uiOutput("ui_years"))
                                       ),
                                       
                                       #box(solidHeader = TRUE,
                                       #    width = 3,
                                       #    valueBoxOutput("anmBox"),
                                       #    valueBoxOutput("utfBox"),
                                       #    valueBoxOutput("bioBox")
                                       box(solidHeader = TRUE,
                                           width = NULL,
                                           checkboxInput(inputId = "anmBox",
                                                         label = "Visa avverkningsanmälningar inom vald tidsperiod",
                                                         value = T),
                                           checkboxInput(inputId = "utfBox",
                                                         label = "Visa utförda avverkningar inom vald tidsperiod"),
                                           checkboxInput(inputId = "bioBox",
                                                         label = "Visa biotopskydd"))),
                                
                                box(solidHeader = TRUE,
                                    width = 7,
                                    leafletOutput(outputId = "map"))
                                
                                
                        )
                )
        ),
        server <- function(input, output, server, session) {
                
                #observeEvent(input$goButton, {
                #        print(anmData())
                #        print(nchar(anmData()))
                #        print(length(anmData()))
                #        print(!is.null(anmData()))
                #})
                
                ### UI
                observe({
                        updateSelectizeInput(session, 'munies',
                                             choices = municipalities,
                                             server = TRUE)
                })
                
                observe({
                        updateSelectizeInput(session, 'latest',
                                             choices = senaste,
                                             server = TRUE)
                })
                
                output$tabset1Selected <- renderText({
                        input$tabset1
                })
                
                #output$ui_muni <- renderUI({
                #       selectInput("munies",
                #                   label = input$tabset1Selected,
                #                   choices = municipalities[[input$county]],
                #                    selected = "Inget")
                #})
                
                ### Data
                anmData <- eventReactive(c(input$munies, input$latest, input$tabset1), {
                        if(input$tabset1 == "tab1" && !is.null(input$latest)) {
                                if(dir.exists(paste("data/anm/", input$latest, "/", sep = ""))) {
                                        read_sf(paste("data/anm/", input$latest, sep = ""))
                                } else{NULL} 
                        } else if(input$tabset1 == "tab2" && !is.null(input$munies)) {
                                if(dir.exists(paste("data/anm/", input$munies, "/", sep = ""))) {
                                        read_sf(paste("data/anm/", input$munies, sep = ""))
                                } else{NULL} 
                        }
                }
                )
                
                
                utfData <- eventReactive(c(input$munies, input$latest, input$tabset1), {
                        if(input$tabset1 == "tab2") {
                                if(file.exists(paste("data/utf/", input$munies, "/", input$munies, ".shp", sep = ""))) {
                                        read_sf(paste("data/utf/", input$munies, sep = ""))
                                }
                                else{NULL}
                        }
                        else{
                                if(file.exists(paste("data/utf/", input$latest, "/", input$latest, ".shp", sep = ""))) {
                                        read_sf(paste("data/utf/", input$latest, sep = ""))
                                }
                                else{NULL} 
                        }
                })
                
                
                bioData <- eventReactive(c(input$munies, input$latest, input$tabset1), {
                        if(input$tabset1 == "tab2") {
                                if(file.exists(paste("data/bio/", input$munies, "/", input$munies, ".shp", sep = ""))) {
                                        read_sf(paste("data/bio/", input$munies, sep = ""))
                                }
                                else{NULL}
                        }
                        else{
                                if(file.exists(paste("data/bio/", gsub("Senaste_", "", input$latest), "/", gsub("Senaste_", "", input$latest), ".shp", sep = ""))) {
                                        read_sf(paste("data/bio/", gsub("Senaste_", "", input$latest), sep = ""))
                                }
                                else{NULL} 
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
                        
                        if(input$anmBox == TRUE && !is.null(anmData())) {
                                filteredAnmData <- anmData()
                                if(input$tabset1 == "tab2") {
                                        filteredAnmData <- filteredAnmData[filteredAnmData$Arendear >= selectedYears[1] & filteredAnmData$Arendear <= selectedYears[2],]
                                }
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
                        else(leafletProxy("map") %>%
                                     clearGroup("anm"))
                        
                        
                        
                        if(input$utfBox == TRUE && !is.null(utfData())) {
                                filteredUtfData <- utfData()
                                if(input$tabset1 == "tab2") {
                                        filteredUtfData <- filteredUtfData[filteredUtfData$Arendear >= selectedYears[1] & filteredUtfData$Arendear <= selectedYears[2],]
                                }
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
                        else(leafletProxy("map") %>%
                                     clearGroup("utf"))
                })
                
                observe({
                        bioSelected <- bioData()
                        
                        if(input$bioBox == TRUE) {
                                if(dir.exists(paste("data/bio/", input$munies, "/", sep = ""))) {
                                        bioSelected <- bioData()
                                        print(nrow(bioSelected))
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
                
                ### BOXES
                
                output$anmBox <- renderValueBox({
                        selectedYears <- c(input$years[1], input$years[2])
                        anmBoxData <- anmData()
                        anmBoxData <- anmBoxData[anmBoxData$Arendear >= selectedYears[1] & anmBoxData$Arendear <= selectedYears[2],]
                        if(is.null(anmBoxData)) {
                                valueBox(paste0("0"),
                                         "Avverkningsanmälningar",
                                         color = "green",
                                         width = 3) }
                        else if(nrow(anmBoxData) == 1) {
                                valueBox(paste0(nrow(utfBoxData)),
                                         "Avverkningsanmälningar",
                                         color = "green",
                                         width = 3) }
                        else{
                                valueBox(paste0(nrow(anmBoxData)),
                                         "Avverkningsanmälningar",
                                         color = "green",
                                         width = 3)
                        }
                })
                
                output$utfBox <- renderValueBox({
                        selectedYears <- c(input$years[1], input$years[2])
                        utfBoxData <- utfData()
                        utfBoxData <- utfBoxData[utfBoxData$Arendear >= selectedYears[1] & utfBoxData$Arendear <= selectedYears[2],]
                        if(is.null(utfBoxData)) {
                                valueBox(paste0("0"),
                                         "Utförda avverkningar",
                                         color = "orange",
                                         width = 3) }
                        else if(nrow(utfBoxData) == 1) {
                                valueBox(paste0(nrow(utfBoxData)),
                                         "Utförda avverkningar",
                                         color = "orange",
                                         width = 3) }
                        else{
                                valueBox(paste0(nrow(utfBoxData)),
                                         "Utförda avverkningar",
                                         color = "orange",
                                         width = 3) }
                })
                
                output$bioBox <- renderValueBox({
                        if(is.null(bioData())) {
                                valueBox(paste0("0"),
                                         "Biotopskyddsområden",
                                         color = "purple",
                                         width = 3)
                        }
                        else if(nrow(bioData()) == 1) {
                                valueBox(paste0(nrow(bioData())),
                                         "Biotopskyddsområde",
                                         color = "purple",
                                         width = 3)
                        }
                        else{
                                valueBox(paste0(nrow(bioData())),
                                         "Biotopskyddsområden",
                                         color = "purple", 
                                         width = 3) }
                })
                
                
        }
)

