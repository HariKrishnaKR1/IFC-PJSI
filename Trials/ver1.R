library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(readr)

# Load the dataset
data <- read_csv("D:/Hari/Datasets/Custom SDA goal 16/Data_Unpivoted_Custom_SDG16 _LDA.csv")

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
      sliderInput(
        "year",
        "Select Year:",
        min = 2000,  # Replace with actual year range if available
        max = 2023,  # Replace with actual year range if available
        value = 2023,
        step = 1,
        sep = ""
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", plotlyOutput("barPlot")),
        tabPanel("Table", DTOutput("dataTable")),
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
  
  # Bar Plot
  output$barPlot <- renderPlotly({
    plot_data <- filtered_data()
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
  
  # Data Table
  output$dataTable <- renderDT({
    plot_data <- filtered_data()
    req(nrow(plot_data) > 0)
    
    plot_data %>%
      select(`State or Union Territory`, Value) %>%
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
