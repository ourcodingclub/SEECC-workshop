# Exploring LPI 2016 data in a Shiny app
# John Godlee (johngodlee@gmail.com)

# Packages ----
library(shiny)
library(dplyr)
library(ggmap)

# Load data ----
load("data/LPIdata_Feb2016.RData", envir = .GlobalEnv)
load("data/LPIGBIFUK.RData", envir = .GlobalEnv)
map_world <- borders("world", colour="black", fill = "gray28")

# Define UI ----
ui <- navbarPage(title = "Living Planet Index",
                 tabPanel(title = "LPI Subsets",
                          plotOutput("pop_vs_trend", width = 500),
                          plotOutput("range_vs_trend", width = 500),
                          selectInput(inputId = "realm_pop_vs_trend",
                                      label = "Realm",
                                      choices = unique(LPIGBIFUK$realm),
                                      multiple = TRUE
                                      )
                          ),
                 tabPanel(title = "Global Map",
                          plotOutput("global_map", height = "600px"),  # Reserve space for legend in plot panel so the map doesn't change size
                          selectInput(inputId = "realm_map", 
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
                 data = LPIdata_Feb2016[LPIdata_Feb2016$realm == input$realm_map,]) + 
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
  
  output$pop_vs_trend <- renderPlot(
    ggplot() + 
      geom_point(aes(x = log(meanpop.size), 
                     y = slope, 
                     colour = realm),
                 data = LPIGBIFUK[LPIGBIFUK$realm == input$realm_pop_vs_trend,]) + 
      geom_pointrange(aes(x = log(meanpop.size), 
                          y = slope, 
                          ymin = slope - slope_SE, 
                          ymax = slope+slope_SE, 
                          colour = realm),
                      data = LPIGBIFUK[LPIGBIFUK$realm == input$realm_pop_vs_trend,]) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      xlim(0, 20) +
      ylim(-.15, .15) +
      theme_classic() + 
      theme(
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.border = element_rect(colour="black", fill = NA, size=1),
            panel.background = element_rect(fill="#FCFCFC"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 15))
      )
  output$range_vs_trend <- renderPlot(ggplot() +
    geom_point(aes(x = km2_range, 
                   y = slope, 
                   colour = realm),
               data = LPIGBIFUK[LPIGBIFUK$realm == input$realm_pop_vs_trend,]) +
    geom_pointrange(aes(x = km2_range, 
                        y = slope, 
                        ymin = slope - slope_SE, 
                        ymax = slope + slope_SE,
                        colour = realm),
                    data = LPIGBIFUK[LPIGBIFUK$realm == input$realm_pop_vs_trend,]) +
    geom_hline(yintercept = 0, linetype="dashed") + 
      theme_classic() + 
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(colour="black", fill = NA, size=1),
        panel.background = element_rect(fill="#FCFCFC"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15))
    )
}
# Run application ----
shinyApp(ui = ui, server = server)
