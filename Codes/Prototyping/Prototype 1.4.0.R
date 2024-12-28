# Required Libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(DT)

# Load Data
load(url("https://raw.githubusercontent.com/HariKrishnaKR1/IFC-PJSI/main/Data/Files.RData"))

# Data Preparation
data$State <- as.character(data$State)
india_states$State <- as.character(india_states$State)
unique_targets <- c("Please choose a target to display here", sort(as.numeric(unique(data$Target.Number)), na.last = NA))

# Transform Spatial Data
india_states <- st_transform(india_states, crs = 4326)

# Shiny App Setup
ui <- fluidPage(
  titlePanel("Interactive SDG Dashboard"),
  
  fluidRow(
    column(
      width = 3,
      selectInput(
        inputId = "target",
        label = "Select Target:",
        choices = unique_targets,
        selected = "Please choose a target to display here"
      )
    )
  ),
  
  fluidRow(
    column(
      width = 6,
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("State Chart", plotOutput("state_bar_chart")),
        tabPanel("State Table", DTOutput("state_table"))
      )
    ),
    
    column(
      width = 6,
      tabsetPanel(
        tabPanel("Indicator Chart", plotOutput("indicator_bar_chart")),
        tabPanel("Indicator Table", DTOutput("indicator_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive Data for Selected Target
  filtered_data <- reactive({
    if (is.null(input$target) || input$target == "Please choose a target to display here") {
      data %>%
        group_by(Target.Number) %>%
        summarise(
          Description = first(Description),
          Average_Score = round(mean(Final.Value, na.rm = TRUE), 3)
        )
    } else {
      data %>%
        filter(Target.Number == input$target) %>%
        group_by(Indicator.Number) %>%
        summarise(
          Description = first(Description),
          Average_Score = round(mean(Final.Value, na.rm = TRUE), 3)
        )
    }
  })
  
  # Map Plot
  output$map <- renderLeaflet({
    if (is.null(input$target) || input$target == "Please choose a target to display here") {
      target_data <- data %>%
        group_by(State) %>%
        summarise(Average_Value = mean(Final.Value, na.rm = TRUE))
      legend_title <- "Average Value (All Targets)"
    } else {
      target_data <- data %>%
        filter(Target.Number == input$target) %>%
        group_by(State) %>%
        summarise(Average_Value = mean(Final.Value, na.rm = TRUE))
      legend_title <- paste("Average Value for Target", input$target)
    }
    
    india_states_joined <- india_states %>%
      left_join(target_data, by = "State")
    
    pal <- colorNumeric(palette = colorRampPalette(c("#FF0000", "#FFFF00", "#017D13"))(100), 
                        domain = c(0, 100), na.color = "grey")
    
    leaflet(india_states_joined) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Average_Value),
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste(State, ":", round(Average_Value, 2), "%"),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = india_states_joined$Average_Value, title = legend_title, labFormat = labelFormat(suffix = "%"))
  })
  
  # State Ranking Bar Chart
  output$state_bar_chart <- renderPlot({
    state_rankings <- if (is.null(input$target) || input$target == "Please choose a target to display here") {
      data %>%
        group_by(State) %>%
        summarise(Average_Score = mean(Final.Value, na.rm = TRUE)) %>%
        arrange(Average_Score)
    } else {
      data %>%
        filter(Target.Number == input$target) %>%
        group_by(State) %>%
        summarise(Average_Score = mean(Final.Value, na.rm = TRUE)) %>%
        arrange(Average_Score)
    }
    
    ggplot(state_rankings, aes(x = Average_Score, y = reorder(State, Average_Score), fill = Average_Score)) +
      geom_col() +
      scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, limits = c(0, 100)) +
      labs(
        title = "State Rankings",
        x = "Average Score",
        y = "State"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # State Table
  output$state_table <- renderDT({
    state_rankings <- if (is.null(input$target) || input$target == "Please choose a target to display here") {
      data %>%
        group_by(State) %>%
        summarise(Average_Score = round(mean(Final.Value, na.rm = TRUE), 2))
    } else {
      data %>%
        filter(Target.Number == input$target) %>%
        group_by(State) %>%
        summarise(Average_Score = round(mean(Final.Value, na.rm = TRUE), 2))
    }
    
    datatable(state_rankings, options = list(autoWidth = TRUE, pageLength = 10))
  })
  
  # Indicator Bar Chart
  output$indicator_bar_chart <- renderPlot({
    if (input$target == "Please choose a target to display here") {
      target_scores <- data %>%
        group_by(Target.Number) %>%
        summarise(Average_Score = round(mean(Final.Value, na.rm = TRUE), 3))
      
      ggplot(target_scores, aes(x = Average_Score, y = reorder(Target.Number, Average_Score), fill = Average_Score)) +
        geom_col() +
        scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, limits = c(0, 100)) +
        labs(
          title = "Average Scores by Target",
          x = "Average Score",
          y = "Target Number"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      indicator_scores <- filtered_data()
      
      ggplot(indicator_scores, aes(x = Average_Score, y = reorder(Indicator.Number, Average_Score), fill = Average_Score)) +
        geom_col() +
        scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, limits = c(0, 100)) +
        labs(
          title = paste("Indicator Scores for Target", input$target),
          x = "Average Score",
          y = "Indicator Number"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
  
  # Indicator Table
  output$indicator_table <- renderDT({
    indicator_summary <- filtered_data()
    datatable(indicator_summary, options = list(autoWidth = TRUE, pageLength = 10))
  })
}

# Run the App
shinyApp(ui, server)
