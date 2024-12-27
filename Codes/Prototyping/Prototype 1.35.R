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

# Preprocess Distinct Tables
distinct_tables <- lapply(unique_targets[-1], function(target) {
  target_data <- data %>% filter(Target.Number == target)
  distinct_indicators <- target_data %>% select(Indicator.Number, Description) %>% distinct()
  return(distinct_indicators)
})
names(distinct_tables) <- unique_targets[-1]

# Shiny App Setup
ui <- fluidPage(
  titlePanel("Interactive SDG Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "target",
        label = "Select Target:",
        choices = unique_targets,
        selected = "Please choose a target to display here"
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
    if (input$target == "Please choose a target to display here") {
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
    if (input$target == "Please choose a target to display here") {
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
    if (input$target == "Please choose a target to display here") {
      state_rankings <- data %>%
        group_by(State) %>%
        summarise(Average_Score = mean(Final.Value, na.rm = TRUE)) %>%
        arrange(Average_Score)
      chart_title <- "State Rankings (All Targets)"
    } else {
      state_rankings <- data %>%
        filter(Target.Number == input$target) %>%
        group_by(State) %>%
        summarise(Average_Score = mean(Final.Value, na.rm = TRUE)) %>%
        arrange(Average_Score)
      chart_title <- paste("State Rankings for Target", input$target)
    }
    
    ggplot(state_rankings, aes(x = Average_Score, y = reorder(State, Average_Score), fill = Average_Score)) +
      geom_col() +
      scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, limits = c(0, 100)) +
      labs(
        title = chart_title,
        x = "Average Score",
        y = "State"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
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
    if (input$target == "Please choose a target to display here") {
      target_summary <- data %>%
        group_by(Target.Number) %>%
        summarise(
          Description = first(Description),
          Average_Score = round(mean(Final.Value, na.rm = TRUE), 3)
        ) %>%
        arrange(Target.Number)
      table_title <- "Target Summary"
    } else {
      indicator_summary <- filtered_data()
      table_title <- paste("Indicator Summary for Target", input$target)
    }
    
    datatable(
      if (input$target == "Please choose a target to display here") target_summary else indicator_summary,
      colnames = c("ID", "Description", "Average Score"),
      options = list(
        autoWidth = TRUE,
        pageLength = if (input$target == "Please choose a target to display here") 5 else 10
      )
    )
  })
}

# Run the App
shinyApp(ui, server)
