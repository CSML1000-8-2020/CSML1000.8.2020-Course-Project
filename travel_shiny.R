# Load Libraries
# library(knitr)
# library(arules)
# library(arulesViz)
library('dplyr')
library('data.table')
library(shinydashboard)
library(shiny)
library(maps)
library(ggplot2)
#if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
#if(!require(RColorBrewer)) install.packages
library(ggiraph)
library(RColorBrewer)
library(rgeos)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

# Load files
cities.iata <- read.csv("./input/cities_IATA_long_lat.csv", header=TRUE)

unzip("./input/acme-travel-vacation.zip", exdir = "input") # unzip file
raw = read.table("./input/acme-travel-vacation.csv", sep="\t", header=TRUE)

raw$PACKAGE_ID <- NULL
raw$ACCOM_SIZE_PAID <- NULL
raw$ACCOM_LEVEL_PAID <- NULL
raw$ACCOM_LEVEL_RECEIVED <- NULL
raw$ACCOM_TYPE_RECEIVED <- NULL
raw$ACCOM_SIZE_RECEIVED <- NULL
raw$STATUS <- NULL
raw$RATE_HEADER_ID <- NULL
raw$RATE_CODE <- NULL
raw$LIST_PRICE <- NULL
raw$MARKET <- NULL
raw$RATE_GRP <- NULL
raw$CXL_DATE <- NULL
raw$SEND_DATE <- NULL
raw$BKG_ID <- NULL
raw$AGENCY <- NULL
raw$PACKAGE_TYPE_INDICATOR <- NULL
raw$ACCOM_TYPE_PAID <- NULL
raw$TRUE_ORIGIN <- NULL
raw$MAIN_FLIGHT_ORIGIN <- NULL
#raw$MAIN_FLIGHT_DESTINATION <- NULL
raw$INBOUND_FLIGHT_NUMBER <- NULL
raw$INBOUND_FEEDER_FLIGHT_NUMBER <- NULL
raw$INBOUND_TRAVEL_CLASS <- NULL
raw$OUTBOUND_FLIGHT_NUMBER <- NULL
raw$OUTBOUND_FEEDER_FLIGHT_NUMBER <- NULL
raw$OUTBOUND_TRAVEL_CLASS <- NULL
raw$BKG_TYPE <- NULL
raw$SURCHARGES_AND_TAXES <- NULL
raw$ANCILLARY_REVENUE <- NULL
raw$TOTAL_COST <- NULL
raw$SOURCE <- NULL

cities.iata %>% mutate_if(is.factor, as.character) -> ds_cities.iata

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
  dashboardBody(title = "Group 8 Market", 
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
          plotOutput('g')
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
        updateDataset()
        paste("Selected Date Range is", 
              paste(as.character(input$dateRange), collapse = " to ")
        )
      })
      
      output$tlb <- renderTable({
        updateDataset()
        session$userData$ds_dataset
      })
      
      observeEvent(input$getList, {
        updateDataset()
        renderPlot()
      })
      
      output$g <- renderPlot({
        getPlot()
        # updateDataset()
        # df <- merge(session$userData$ds_dataset, cities.iata, by.x="MAIN_FLIGHT_DESTINATION", by.y="IATA")
        # sites <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326,  agr = "constant")
        
        # ggplot() +
        # geom_polygon_interactive(data = world, color = 'gray70', size = 0.1,
        #                          aes(x = "Longitude", y = "Latitude", fill = "Longitude", group = group ))
        
        # ggplot(data = world) +
        

          # geom_sf(fill = "antiquewhite1") +
          # geom_sf(data = sites, size = 2, shape = 23, fill = "darkred") +
          # # annotate("point", x = -80, y = 35, colour = "green", size = 4) +
          # # annotate(geom = "text", x = -80, y = 36, label = "Florida" , 
          # # fontface = "italic", color = "red", size = 2) +
          # coord_sf(xlim = c(-100, -55), ylim = c(5, 25), expand = FALSE) +
          # xlab("Longitude") + ylab("Latitude") +
          # ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))
      })
      
      getPlot <- reactive({
        val <- input$dateRange;
        updateDataset()
        df <- merge(session$userData$ds_dataset, cities.iata, by.x="MAIN_FLIGHT_DESTINATION", by.y="IATA")
        sites <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326,  agr = "constant")
        
        g <- ggplot(data = world) +
          # geom_polygon_interactive(data = world, color = 'gray70', size = 0.1,
          #                           aes(x = "Longitude", y = "Latitude", fill = "Longitude", group = group )) +
          
          geom_sf(fill = "antiquewhite1") +
          geom_sf(data = sites, size = 2, shape = 23, fill = "darkred") +
          # annotate("point", x = -80, y = 35, colour = "green", size = 4) +
          # annotate(geom = "text", x = -80, y = 36, label = "Florida" , 
          # fontface = "italic", color = "red", size = 2) +
          coord_sf(xlim = c(-100, -55), ylim = c(5, 25), expand = FALSE) +
          xlab("Longitude") + ylab("Latitude") +
          ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))
        
        return (g)
      })
      
      updateDataset <- function()
      {
        session$userData$dataset = raw[sample(nrow(raw), 20), ]
        session$userData$ds_dataset <- session$userData$dataset %>% mutate_if(is.factor, as.character)
      }
      # Create the interactive world map
      # output$distPlot <- renderGirafe({
      #   ggiraph(code = print(worldMaps(df, world_data, input$data_type, input$period, input$indicator)))
        # ggiraph(code = print(worldMaps(df, world_data, input$data_type, input$period, input$indicator)))
      # })
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)
