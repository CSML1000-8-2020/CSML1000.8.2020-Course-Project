# Load Libraries
library(dplyr)
library(data.table)
library(shiny)
library(shinydashboard)
library(leaflet)        # javascript mapping lib :)
library(htmltools)      # tools to support html workflow
library(leaflet.extras) # extending the leaflet.jsye
library(rnaturalearth)
library("rnaturalearthdata")
library(rgeos)
library(sf)

# Load files
unzip("./input/acme-travel-vacation.zip", exdir = "input") # unzip file
raw = fread("./input/acme-travel-vacation.csv", sep="\t", header=TRUE)
raw <- raw %>% select(DESTINATION,PROPERTY_ID,PARTY_SIZE,MAIN_FLIGHT_DESTINATION,START_DATE,LENGTH_OF_STAY,BKG_DATE,REVENUE,MARGIN,ACCOMMODATION_STAR_RATING,HOTEL_CJAIN_AFFILIATION)

cities.iata <- fread("./input/cities_IATA_long_lat.csv", header=TRUE)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Group 8 Agency"),
  dashboardSidebar(
    #selectInput('aisle_col', 'Aisle', aisles$aisle)
    dateRangeInput('dateRange',
                   label = 'Search Period: yyyy-mm-dd',
                   start = Sys.Date(), end = Sys.Date() + 2, width='100%'
    ),
    
    actionButton("getList", "Get List")
  ),
  dashboardBody(title = "Group 8 Agency", 
                # Boxes need to be put in a row (or column)
                fluidRow(
                  box(width=12,
                      column(12,
                             verbatimTextOutput("dateRangeText")
                      )
                  )
                ),
                fluidRow(
                  box(width=12,
                      leafletOutput('g')
                  )
                ),
                fluidRow(
                  box(title = "Top Destinations for Bulk Purchase:", width=12,
                      tableOutput(outputId = "tlb"),
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      )
                  )
                )
                
  )
)

# Define server function
server <- function(input, output, session) {
  
  session$userData$dataset <- raw[sample(nrow(raw), 50), ]
  session$userData$ds_dataset <- session$userData$dataset %>% mutate_if(is.factor, as.character)
  
  output$dateRangeText  <- renderText({
    paste("Selected Date Range is",  paste(as.character(input$dateRange), collapse = " to "))
  })
  
  observe({
    output$g <- renderLeaflet({
      getPlot()
    })
  })
  
  getPlot <- reactive({
    val <- input$dateRange
    val2 <- input$getList
    updateDataset()
    df <- merge(session$userData$ds_dataset, cities.iata, by.x="MAIN_FLIGHT_DESTINATION", by.y="IATA")
    sites <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326,  agr = "constant")
    
    g <- leaflet::leaflet(data = sites) %>% # create leaflet object
      leaflet::addTiles() %>% # add basemap
      leaflet.extras::addResetMapButton() %>% 
      leaflet::addMarkers(popup = ~htmltools::htmlEscape(paste(DESTINATION,', Margin: ',MARGIN))) # add data layer - markers
    
    return (g)
  })
  
  updateDataset <- function()
  {
    session$userData$dataset = raw[sample(nrow(raw), 10), ]
    session$userData$ds_dataset <- session$userData$dataset %>% mutate_if(is.factor, as.character)
    
    output$tlb <- renderTable({
      session$userData$ds_dataset
    })
  }
}

# Run Shiny App
shinyApp(ui = ui, server = server)
