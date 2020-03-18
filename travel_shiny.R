# Load Libraries
library(dplyr)
library(data.table)
library(shinydashboard)
library(shiny)
library(maps)
library(ggplot2)
library(ggiraph)
library(RColorBrewer)
library(rgeos)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)        # javascript mapping lib :)
library(htmltools)      # tools to support html workflow
library(leaflet.extras) # extending the leaflet.js

# Load files

raw = fread("./input/acme-travel-vacation.csv", sep="\t", header=TRUE)
raw <- raw %>% select(DESTINATION,PROPERTY_ID,PARTY_SIZE,MAIN_FLIGHT_DESTINATION,START_DATE,LENGTH_OF_STAY,BKG_DATE,REVENUE,MARGIN,ACCOMMODATION_STAR_RATING,HOTEL_CJAIN_AFFILIATION)

cities.iata <- fread("./input/cities_IATA_long_lat.csv", header=TRUE)
ds_cities.iata <- cities.iata %>% mutate_if(is.factor, as.character)  

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
                      #   plotOutput('g')
                      #   title = "Products:",
                      #   selectInput('prod_col', NULL, products_for_dept$product_name, size = 10, selectize = FALSE),
                      # ),
                      # box(
                      #   title = "Cart:",
                      #   selectInput('cart_col', NULL, products_in_cart$product_name, size = 10, selectize = FALSE),
                  )
                ),
                fluidRow(
                  box(title = "Top Destinations for Bulk Purchase:", width=12,
                      # Output: interactive world map
                      # girafeOutput("distPlot")
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
    #sites["name"] <- sites$DESTINATION
    
    g <- leaflet::leaflet(data = sites) %>% # create leaflet object
      leaflet::addTiles() %>% # add basemap
      leaflet::addMarkers(popup = ~htmltools::htmlEscape(paste(DESTINATION,', Margin: ',MARGIN))) # add data layer - markers
    
    # g <- ggplot(data = world) +
    #   # geom_polygon_interactive(data = world, color = 'gray70', size = 0.1,
    #   #                           aes(x = "Longitude", y = "Latitude", fill = "Longitude", group = group )) +
    #   
    #   geom_sf(fill = "antiquewhite1") +
    #   geom_sf(data = sites, size = 2, shape = 23, fill = "darkred") +
    #   # annotate("point", x = -80, y = 35, colour = "green", size = 4) +
    #   # annotate(geom = "text", x = -80, y = 36, label = "Florida" , 
    #   # fontface = "italic", color = "red", size = 2) +
    #   coord_sf(xlim = c(-100, -55), ylim = c(5, 25), expand = FALSE) +
    #   xlab("Longitude") + ylab("Latitude") +
    #   ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))
    
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
  # Create the interactive world map
  # output$distPlot <- renderGirafe({
  #   ggiraph(code = print(worldMaps(df, world_data, input$data_type, input$period, input$indicator)))
  # ggiraph(code = print(worldMaps(df, world_data, input$data_type, input$period, input$indicator)))
  # })
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)
