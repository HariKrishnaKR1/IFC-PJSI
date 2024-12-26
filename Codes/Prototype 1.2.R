# Required Libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(DT)

# Load Data
load(url("https://raw.githubusercontent.com/HariKrishnaKR1/ICF-PJSI/main/Data/Files.RData"))

# Data Preparation
data$State <- as.character(data$State)
india_states$State <- as.character(india_states$State)
unique_targets <- unique(data$Target.Number)

# Transform Spatial Data
india_states <- st_transform(india_states, crs = 4326)

# Preprocess Distinct Tables
distinct_tables <- lapply(unique_targets, function(target) {
  target_data <- data %>% filter(Target.Number == target)
  distinct_indicators <- target_data %>% select(Indicator.Number, Description) %>% distinct()
  return(distinct_indicators)
})
names(distinct_tables) <- unique_targets

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
      plotOutput("indicator_bar_chart"),
      DTOutput("indicator_table")
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
    
    pal <- colorNumeric(palette = colorRampPalette(c("#FF0000", "#FFFF00", "#017D13"))(100), 
                        domain = c(0, 100), na.color = "grey")
    
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
      addLegend(pal = pal, values = c(0, 100), title = "Average Value", labFormat = labelFormat(suffix = "%"))
  })
  
  # State Ranking Bar Chart
  output$state_bar_chart <- renderPlot({
    state_rankings <- filtered_data() %>%
      group_by(State) %>%
      summarise(Average_Score = mean(Final.Value, na.rm = TRUE)) %>%
      arrange(desc(Average_Score))
    
    ggplot(state_rankings, aes(x = Average_Score, y = reorder(State, Average_Score), fill = Average_Score)) +
      geom_col() +
      scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, limits = c(0, 100)) +
      labs(
        title = paste("State Rankings for Target", input$target),
        x = "Average Score",
        y = "State"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Indicator Bar Chart
  output$indicator_bar_chart <- renderPlot({
    indicator_scores <- filtered_data() %>%
      group_by(Indicator.Number) %>%
      summarise(Average_Score = mean(Final.Value, na.rm = TRUE))
    
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
  })
  
  # Indicator Table
  output$indicator_table <- renderDT({
    datatable(
      distinct_tables[[as.character(input$target)]],
      colnames = c("Indicator ID", "Description"),
      options = list(
        autoWidth = TRUE,
        pageLength = 10,
        columnDefs = list(
          list(targets = 0, className = "dt-left", width = '20%'),
          list(targets = 1, className = "dt-left", render = JS(
            "function(data, type, row) {return type === 'display' && data.length > 50 ? data.substr(0, 50) + '...' : data;}"
          ))
        )
      )
    )
  })
}

# Run the App
shinyApp(ui, server)
