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

# Load files needed for selecting products
raw = read.table("./input/acme-travel-vacation.csv", sep="\t", header=TRUE)
set.seed(1234)
raw_sample = raw[sample(nrow(raw), 20), ]

world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
wd_head <- head(world_data, 12)
str(wd_head)
summary(wd_head)

worldMaps <- function(df, world_data, data_type, period, indicator){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- df[df$Indicator == indicator & df$DataType == data_type & df$Period == period,]
  plotdf <- plotdf[!is.na(plotdf$ISO3), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['DataType'] <- rep(data_type, nrow(world_data))
  world_data['Period'] <- rep(period, nrow(world_data))
  world_data['Indicator'] <- rep(indicator, nrow(world_data))
  world_data['Value'] <- plotdf$Value[match(world_data$ISO3, plotdf$ISO3)]
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: ", ifelse(data_type == "Childlessness", "United Nations" , "World Bank"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    annotate("point", x = -40, y = 35, colour = "green", size = 4) +
    labs(fill = data_type, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}

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
      box(title = "Top Performing Destinations:", width=12,
          # Output: interactive world map
          girafeOutput("distPlot")
      #tableOutput(outputId = "product_added_to_cart"),
      #   tags$style(type="text/css",
      #             ".shiny-output-error { visibility: hidden; }",
      #             ".shiny-output-error:before { visibility: hidden; }"
      #             )
      )
    )

  )
)

# Define server function
server <- function(input, output, session) {
  
      output$dateRangeText  <- renderText({
        paste("input$dateRange is", 
              paste(as.character(input$dateRange), collapse = " to ")
        )
      })
      
      output$g <- renderPlot({ ggplot() + 
        geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                                 aes(x = long, y = lat, fill = lat, group = group )
        )
      })
      
      # Create the interactive world map
      output$distPlot <- renderGirafe({
        ggiraph(code = print(worldMaps(df, world_data, input$data_type, input$period, input$indicator)))
        # ggiraph(code = print(worldMaps(df, world_data, input$data_type, input$period, input$indicator)))
      })
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)


