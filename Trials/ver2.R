library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(readr)
library(sf)
library(leaflet)

# Load the dataset
data <- read_csv("Data_Unpivoted_Custom_SDG16_LDA.csv")

# Load shapefile
shapefile <- st_read("India_State_Boundary.shx")

# Define UI
ui <- fluidPage(
  titlePanel("Custom SDG16 Indicators Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "target",
        "Select Target:",
        choices = unique(data$`Target Number`),
        selected = unique(data$`Target Number`)[1]
      ),
      selectInput(
        "indicator",
        "Select Indicator:",
        choices = NULL
      ),
      checkboxInput(
        "average",
        "Show Average Value",
        value = FALSE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard", 
                 fluidRow(
                   column(6, leafletOutput("mapPlot")),
                   column(6, plotlyOutput("barPlot"))
                 ),
                 fluidRow(
                   column(12, DTOutput("subindicatorTable"))
                 )
        ),
        tabPanel("Trend", plotlyOutput("trendPlot"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Update indicators based on target selection
  observeEvent(input$target, {
    target_data <- data %>% filter(`Target Number` == input$target)
    updateSelectInput(
      session,
      "indicator",
      choices = unique(target_data$`Indicator Number`)
    )
  })
  
  # Filter data based on inputs
  filtered_data <- reactive({
    req(input$indicator, input$target)
    data %>%
      filter(
        `Target Number` == input$target,
        `Indicator Number` == input$indicator
      )
  })
  
  # Calculate average value if checkbox is selected
  averaged_data <- reactive({
    if (input$average) {
      filtered_data() %>%
        group_by(`State or Union Territory`) %>%
        summarize(Value = mean(Value, na.rm = TRUE), .groups = 'drop')
    } else {
      filtered_data()
    }
  })
  
  # Bar Plot
  output$barPlot <- renderPlotly({
    plot_data <- averaged_data()
    req(nrow(plot_data) > 0)
    
    p <- ggplot(plot_data, aes(x = reorder(`State or Union Territory`, -Value), y = Value, fill = `State or Union Territory`)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = paste("Indicator:", input$indicator),
        x = "State or Union Territory",
        y = "Value"
      )
    
    ggplotly(p)
  })
  
  # Map Plot
  output$mapPlot <- renderLeaflet({
    map_data <- averaged_data()
    req(nrow(map_data) > 0)
    
    map_data <- shapefile %>%
      left_join(map_data, by = c("State_Name" = "State or Union Territory"))
    
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", Value)(Value),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(State_Name, Value)
      )
  })
  
  # Subindicators Table
  output$subindicatorTable <- renderDT({
    table_data <- averaged_data()
    req(nrow(table_data) > 0)
    
    table_data %>%
      select(`State or Union Territory`, `Indicator Number`, Description, Value) %>%
      datatable(
        options = list(pageLength = 10),
        rownames = FALSE
      )
  })
  
  # Trend Plot
  output$trendPlot <- renderPlotly({
    trend_data <- data %>%
      filter(
        `Target Number` == input$target,
        `Indicator Number` == input$indicator
      )
    req(nrow(trend_data) > 0)
    
    p <- ggplot(trend_data, aes(x = `Processed Value`, y = Value, color = `State or Union Territory`)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(
        title = paste("Trend for Indicator:", input$indicator),
        x = "Processed Value",
        y = "Value"
      )
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
