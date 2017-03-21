# Exploring LPI 2016 data in a Shiny app
# John Godlee (johngodlee@gmail.com)

# Packages ----
library(shiny)
library(dplyr)
library(ggmap)

# Load data ----
load("data/LPIdata_Feb2016.RData", envir=.GlobalEnv)
map_world <- borders("world", colour="black", fill = "gray28")

# Define UI ----
ui <- navbarPage(title = "Living Planet Index",
                 tabPanel(title = "LPI Subsets"),
                 tabPanel(title = "Global Map",
                          plotOutput("global_map", height = "600px"),
selectInput(inputId = "realm", 
            label = "Realm", 
            choices = unique(LPIdata_Feb2016$realm),
            multiple = TRUE
            )
)
)
# Define server logic ----
server <- function(input, output) {

  # Plot map
  output$global_map <- renderPlot(
    ggplot() + 
      map_world +
      geom_point(aes(x = Decimal_Longitude, 
                     y = Decimal_Latitude, 
                     colour = realm), 
                 data = LPIdata_Feb2016[LPIdata_Feb2016$realm == input$realm,]) + 
      theme_classic() + 
      theme(axis.title = element_blank(), 
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.border = element_rect(colour="black", fill = NA, size=1),
            panel.background = element_rect(fill="#FCFCFC"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 15)) +
      guides(colour = guide_legend(override.aes = list(size = 10)))
      )
}
# Run application ----
shinyApp(ui = ui, server = server)
