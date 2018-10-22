
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
                                column(width = 4,
                                       tabBox(width = NULL,
                                              height = "210px",
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
                                              tabPanel("Sök historisk data",
                                                       value = "tab2",
                                                       selectizeInput("munies",
                                                                      label = "",
                                                                      choices = NULL,
                                                                      selected = NULL,
                                                                      options = list(placeholder = 'Sök kommun...')),
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
                                    width = 8,
                                    leafletOutput(outputId = "map"))
                                
                                
                        )
                )
        ),
        server <- function(input, output, server, session) {
                
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
                
                anmData <- eventReactive(c(input$munies, input$latest, input$tabset1), {
                        if(input$tabset1 == "tab1" && input$latest != "") {
                                if(dir.exists(paste("data/anm/", input$latest, "/", sep = ""))) {
                                        read_sf(paste("data/anm/", input$latest, sep = ""))
                                } else{NULL} 
                        } else if(input$tabset1 == "tab2" && input$munies != "") {
                                if(dir.exists(paste("data/anm/", input$munies, "/", sep = ""))) {
                                        read_sf(paste("data/anm/", input$munies, sep = ""))
                                } else{NULL} 
                        }
                })
                
                
                utfData <- eventReactive(c(input$munies, input$latest, input$tabset1), {
                        if(input$tabset1 == "tab1" && input$latest != "") {
                                if(dir.exists(paste("data/utf/", input$latest, "/", sep = ""))) {
                                        read_sf(paste("data/utf/", input$latest, sep = ""))
                                } else{NULL} 
                        } else if(input$tabset1 == "tab2" && input$munies != "") {
                                if(dir.exists(paste("data/utf/", input$munies, "/", sep = ""))) {
                                        read_sf(paste("data/utf/", input$munies, sep = ""))
                                } else{NULL} 
                        }
                })
                
                bioData <- eventReactive(c(input$munies, input$latest, input$tabset1), {
                        if(input$tabset1 == "tab1" && input$latest != "") {
                                if(dir.exists(paste("data/bio/", gsub("Senaste_", "", input$latest)))) {
                                        read_sf(paste("data/bio/", gsub("Senaste_", "", input$latest), sep = ""))
                                } else{NULL} 
                        } else if(input$tabset1 == "tab2" && input$munies != "") {
                                if(dir.exists(paste("data/bio/", input$munies, "/", sep = ""))) {
                                        read_sf(paste("data/bio/", input$munies, sep = ""))
                                } else{NULL} 
                        }
                })
                

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
                                                    popup = paste("<b>Inkomstdatum:</b>", filteredAnmData$Inkomdatum, tags$br(),
                                                                  "<b>Skogstyp:</b>", filteredAnmData$Skogstyp, tags$br(),
                                                                  "<b>Avverkningstyp:</b>", filteredAnmData$Avverktyp, tags$br(),
                                                                  "<b>Anmäld HA:</b>", filteredAnmData$Anmaldha, tags$br(),
                                                                  "<b>Skogsodlha:</b>", filteredAnmData$Skogsodlha, tags$br(),
                                                                  "<b>Natforha:</b>", filteredAnmData$Natforha, tags$br(),
                                                                  "<b>Avvha:</b>", filteredAnmData$Avvha, tags$br()
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
                                                    popup = paste("<b>Avverkningsdatum:</b>", filteredUtfData$Avvdatum, tags$br(),
                                                                  "<b>Skogstyp:</b>", filteredUtfData$Skogstyp, tags$br(),
                                                                  "<b>Avverkningstyp:</b>", filteredUtfData$Avverktyp, tags$br(),
                                                                  "<b>Anmäld avverkningsyta:</b>", filteredUtfData$AnmaldHa, tags$br(),
                                                                  "<b>Skogsodlha:</b>", filteredUtfData$SkogsodlHa, tags$br(),
                                                                  "<b>Natforha:</b>", filteredUtfData$Natforha, tags$br(),
                                                                  "<b>Arealha:</b>", filteredUtfData$Arealha, tags$br()
                                                    ))
                        }
                        else(leafletProxy("map") %>%
                                     clearGroup("utf"))
                })
                
                observe({
                        bioSelected <- bioData()
                        
                        if(input$bioBox == TRUE && !is.null(bioSelected)) {
                                leafletProxy("map", data = bioSelected) %>%
                                        clearGroup("bio") %>%
                                        addPolygons(layerId = ~OBJECTID,
                                                    color = "purple",
                                                    group = "bio",
                                                    popup = paste("<b>Biotopkategori:</b>", bioSelected$Biotyp, tags$br(),
                                                                  "<b>Skogstyp:</b>", bioSelected$Naturtyp, tags$br(),
                                                                  "<b>Beslutsdatum:</b>", bioSelected$Datbeslut, tags$br(),
                                                                  "<b>Produktiv skogsmark (ha):</b>", bioSelected$Areaprod, tags$br(),
                                                                  "<b>Total areal (ha):</b>", bioSelected$Areato, tags$br(),
                                                                  "<b>Standort:</b>", bioSelected$Standort, tags$br(),
                                                                  "<b>Url:</b>", bioSelected$Url, tags$br(),
                                                                  "<b>Biotopkategori:</b>", bioSelected$Biotyp, tags$br())
                                                    )
                        }
                        else(leafletProxy("map") %>%
                                     clearGroup("bio"))
                })
                
        }
)

