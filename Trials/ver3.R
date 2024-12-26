# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)

# Load and preprocess data
data <- read_csv("D:/Hari/Datasets/Custom SDA goal 16/Data_Unpivoted_Custom_SDG16 _LDA.csv")

# Load shapefiles for state boundaries
india_states <- st_read("D:/Hari/Coding/GIS/QGIS/India-State-and-Country/India-State-and-Country/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")

# Standardize column names for joining
data <- data %>% rename(STATE_NAME = `State or Union Territory`)
data$STATE_NAME <- trimws(toupper(data$STATE_NAME))
india_states$STATE_NAME <- trimws(toupper(india_states$State_Name))

# Merge data with spatial information
merged_data <- india_states %>%
  left_join(data, by = "STATE_NAME") %>%
  filter(!is.na(Score))  # Ensure Score column is not NA

# Define UI
ui <- fluidPage(
  titlePanel("SDG 16 Dashboard: Peace, Justice, and Strong Institutions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Select Indicator:", choices = unique(data$Description)),
      sliderInput("year", "Select Year:", 
                  min = min(data$Year, na.rm = TRUE), max = max(data$Year, na.rm = TRUE), value = max(data$Year, na.rm = TRUE), step = 1)
    ),
    mainPanel(
      leafletOutput("map", height = 600),
      plotOutput("bar_chart", height = 400)
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Reactive subset of data based on inputs
  filtered_data <- reactive({
    req(input$indicator, input$year)  # Ensure inputs are available
    merged_data %>%
      filter(Description == input$indicator & Year == input$year)
  })
  
  # Render map
  output$map <- renderLeaflet({
    data_to_plot <- filtered_data()
    if (nrow(data_to_plot) > 0) {
      leaflet(data_to_plot) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~colorQuantile("YlGnBu", Score, n = 5)(Score),
          color = "black", weight = 1, fillOpacity = 0.7,
          label = ~paste0(STATE_NAME, ": ", round(Score, 2))
        )
    } else {
      leaflet() %>% addTiles()
    }
  })
  
  # Render bar chart
  output$bar_chart <- renderPlot({
    data_to_plot <- filtered_data() %>%
      st_drop_geometry()
    
    if (nrow(data_to_plot) > 0) {
      ggplot(data_to_plot, aes(x = reorder(STATE_NAME, -Score), y = Score, fill = Score)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_gradient(low = "orange", high = "green") +
        labs(title = paste("State-wise Scores for", input$indicator, "in", input$year),
             x = "State", y = "Score") +
        theme_minimal()
    } else {
      ggplot() +
        labs(title = "No data available for the selected indicator and year", x = NULL, y = NULL) +
        theme_void()
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
