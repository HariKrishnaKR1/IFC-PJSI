# Required Libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)

# Load Data
data <- read.csv("D:/Hari/Datasets/Custom SDA goal 16/Data_Unpivoted_Custom_SDG16 _LDA.csv")
india_states <- st_read("D:/Hari/Coding/GIS/QGIS/India-State-and-Country/True/India_State_Boundary.shp")

# Data Preparation
data <- data %>% rename(State = State.or.Union.Territory)
data$State <- as.character(data$State)
india_states$State <- as.character(india_states$State)
unique_targets <- unique(data$Target.Number)

# Shiny App Setup
ui <- fluidPage(
  titlePanel("Interactive SDG Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "target",
        label = "Select Target:",
        choices = unique_targets,
        selected = unique_targets[1]
      )
    ),
    
    mainPanel(
      leafletOutput("map"),
      plotOutput("state_bar_chart"),
      plotOutput("indicator_bar_chart")
    )
  )
)

server <- function(input, output, session) {
  # Reactive Data for Selected Target
  filtered_data <- reactive({
    data %>% filter(Target.Number == input$target)
  })
  
  # Map Plot
  output$map <- renderLeaflet({
    target_data <- filtered_data() %>%
      group_by(State) %>%
      summarise(Average_Value = mean(Final.Value, na.rm = TRUE))
    
    india_states <- india_states %>%
      left_join(target_data, by = "State")
    
    pal <- colorNumeric(palette = "Blues", domain = india_states$Average_Value, na.color = "grey")
    
    india_states = sf_transform
    
    leaflet(india_states) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Average_Value),
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste(State, ":", round(Average_Value, 2))
      ) %>%
      addLegend(pal = pal, values = india_states$Average_Value, title = "Average Value")
  })
  
  # State Ranking Bar Chart
  output$state_bar_chart <- renderPlot({
    state_rankings <- filtered_data() %>%
      group_by(State) %>%
      summarise(Average_Score = mean(Final.Value, na.rm = TRUE)) %>%
      arrange(desc(Average_Score))
    
    ggplot(state_rankings, aes(x = reorder(State, Average_Score), y = Average_Score)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("State Rankings for Target", input$target),
        x = "State",
        y = "Average Score"
      ) +
      theme_minimal()
  })
  
  # Indicator Bar Chart
  output$indicator_bar_chart <- renderPlot({
    indicator_scores <- filtered_data() %>%
      group_by(Indicator.Number) %>%
      summarise(Average_Score = mean(Final.Value, na.rm = TRUE))
    
    ggplot(indicator_scores, aes(x = reorder(Indicator.Number, Average_Score), y = Average_Score)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(
        title = paste("Indicator Scores for Target", input$target),
        x = "Indicator Number",
        y = "Average Score"
      ) +
      theme_minimal()
  })
}

# Run the App
shinyApp(ui, server)
