# Exploring LPI 2016 data in a Shiny app
# John Godlee (johngodlee@gmail.com)

# Packages ----
library(shiny)
library(dplyr)
library(ggmap)

# Load data ----
load("data/LPIdata_Feb2016.RData", envir=.GlobalEnv)

# Define UI ----
ui <- navbarPage(title = "Living Planet Index",
                 tabPanel(title = "LPI Subsets"),
                 tabPanel(title = "Global Map",
                          plotOutput("global_map"),
checkboxGroupInput("biome_map", "Choose which biomes are displayed", c("Temperate coastal rivers",
"Tropical and subtropical floodplain rivers and wetland complexes",
"Unknown", 
"Boreal forests/taiga",
"Temperate floodplain rivers and wetlands",
"Temperate broadleaf and mixed forests", 
"Xeric freshwaters and endorheic basins",
"Temperate upland rivers", 
"Polar freshwaters", 
"Tundra",
"Temperate coniferous forests",
"Large lakes", 
"Tropical and subtropical coastal rivers",
"Deserts and xeric shrublands",
"Tropical and subtropical coniferous forests",
"Tropical and subtropical moist broadleaf forests",
"Montane freshwaters",
"Polar seas",
"Montane grasslands and shrublands",
"Tropical and subtropical dry broadleaf forests",
"Temperate upwelling",
"Flooded grasslands and savannas",
"Mangroves",
"Tropical and subtropical upland rivers",
"Tropical coral",
"Large river deltas",
"Oceanic islands",
"Temperate shelves and seas",
"Tropical upwelling",
"Temperate grasslands savannas and shrublands",
"Mediterranean forests woodlands and scrub",
"Tropical and subtropical grasslands savannas and shrublands") ))
                 )

# Define server logic ----
server <- function(input, output) {
  # Creating a map background
  bbox <- c(left = -180, bottom = -70, right = 179, top = 85)
  map <- get_map(bbox, zoom = 3)
  # Plot map
  output$global_map <- renderPlot(ggmap(map)) #+
                                    #geom_point(data = LPIdata_Feb2016[LPIdata_Feb2016$biome == input$biome_map,]))
  
}
# Run application ----
shinyApp(ui = ui, server = server)
