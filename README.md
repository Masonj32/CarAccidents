# CarAccidents
#
# Shiny app by Mason Johnson
# November 4th, 2024
#
# 
# Deployed at https://masonjohnson3822.shinyapps.io/Accident2/
# Source Code at https://github.com/Masonj32/CarAccidents/edit/main/README.md   
#
# This shiny app allows users to look at car accidents per 100k within each region and division
################################################################################################

library(rsconnect)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)


rsconnect::setAccountInfo(name='masonjohnson3822', token='0E735E2BB664B7C62B1DF77C5B6E91D2', secret='nBhJmBn3DpF3v+B3dsAcqGCxz8n8f2rkr2fikYjP')

library(readr)
accident <- read.csv("accident.csv") 
Census_1_ <- read_csv("Census (1).csv")

library(dplyr)

accident3 = accident %>%
  left_join(Census_1_, by = c("STATENAME" = "NAME"))

state_accident_counts <- accident3 %>%
  group_by(STATENAME) %>%
  summarise(accident_count = n())

state_accidents_normalized <- state_accident_counts %>%
  left_join(accident3 %>% select(STATENAME, POPESTIMATE2019) %>% distinct(), by = "STATENAME")

state_accidents_normalized <- state_accidents_normalized %>%
  mutate(accidents_per_100k = (accident_count * 100000) / POPESTIMATE2019)

accident3 = accident3 %>%
  left_join(state_accidents_normalized %>% select(STATENAME, accidents_per_100k), by = "STATENAME")

library(shiny)
library(plotly)  
library(dplyr)
library(maps)


ui <- fluidPage(
  titlePanel("U.S. Accident Rate Heat Map (Per 100k)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select a Region:", 
                  choices = c("All Regions" = "all", "Northeast" = 1, "Midwest" = 2, "South" = 3, "West" = 4),
                  selected = "all"),  # Set default selection to "All Regions"
      
      uiOutput("division_ui")  # Dynamic UI for division selection
    ),
    
    mainPanel(
      plotlyOutput("mapOutput", height = 600)  
    )
  )
)


server <- function(input, output, session) {
  
  output$division_ui <- renderUI({
    req(input$region)
    region_divisions <- list(
      "1" = c("All Divisions" = "", "Division 1" = 1, "Division 2" = 2),
      "2" = c("All Divisions" = "", "Division 3" = 3, "Division 4" = 4),
      "3" = c("All Divisions" = "", "Division 5" = 5, "Division 6" = 6, "Division 7" = 7),
      "4" = c("All Divisions" = "", "Division 8" = 8, "Division 9" = 9)
    )
    divisions <- region_divisions[[as.character(input$region)]]
    
    if (!is.null(divisions)) {
      selectInput("division", "Select a Division:", choices = divisions)
    }
  })
  
 
  output$mapOutput <- renderPlotly({
    
    
    filtered_data <- accident3
    
    if (input$region != "all") {  
      filtered_data <- filtered_data %>% filter(REGION == as.numeric(input$region))
    }
    
    if (!is.null(input$division) && input$division != "") {
      filtered_data <- filtered_data %>% filter(DIVISION == as.numeric(input$division))
    }
    
  
    filtered_data <- filtered_data %>%
      group_by(STATENAME) %>%
      summarise(accidents_per_100k = mean(accidents_per_100k, na.rm = TRUE)) %>%  
      mutate(region = tolower(STATENAME))  
    
    
    us_map <- map_data("state") %>%
      filter(region %in% filtered_data$region) %>% 
      left_join(filtered_data, by = "region")
 
    p <- ggplot(us_map, aes(x = long, y = lat, group = group, fill = accidents_per_100k)) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "lightyellow", high = "red", na.value = "grey", name = "Accidents per 100k") +
      coord_fixed(1.3) +
      labs(title = ifelse(input$region == "all", "U.S. Accident Rate Heat Map",
                          paste("Accident Rate Heat Map - Region", input$region, 
                                ifelse(input$division != "", paste("Division", input$division), "")))) +
      theme_minimal() +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    
 
    ggplotly(p, tooltip = c("fill"))  # Customize tooltips as needed
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)



