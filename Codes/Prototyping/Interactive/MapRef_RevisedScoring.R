# Load necessary libraries
library(ggplot2)
library(dplyr)
library(leaflet)
library(tidyr)
library(sf)
library(plotly)
library(DT)
library(webshot)

# Load Data
load(url("https://raw.githubusercontent.com/HariKrishnaKR1/IFC-PJSI/main/Data/Files.RData"))

# Data Preparation
data$State <- as.character(data$State)
india_states$State <- as.character(india_states$State)
unique_targets <- sort(as.numeric(unique(data$Target.Number)), na.last = NA)

# Transform Spatial Data
india_states <- st_transform(india_states, crs = 4326)
# Function to Create Maps using bottom 3 indicators
mapcreate <- function(data, india_states, unique_targets) {
  outputs <- list()
  
  ### 1. Map for Overall Average Score (using bottom 3 indicators per target)
  overall_data <- data %>%
    # First get average for each indicator within each target and state
    group_by(Target.Number, Indicator.Number, State) %>%
    summarise(Indicator_Avg = mean(Final.Value, na.rm = TRUE), .groups = "drop") %>%
    # For each target and state, get bottom 3 indicators
    group_by(Target.Number, State) %>%
    arrange(Target.Number, State, Indicator_Avg) %>%
    slice_head(n = 3) %>%
    # Calculate final state average using these bottom 3 indicators
    group_by(State) %>%
    summarise(Average_Value = mean(Indicator_Avg, na.rm = TRUE))
  
  india_states_joined <- india_states %>%
    left_join(overall_data, by = "State")
  
  pal <- colorNumeric(
    palette = colorRampPalette(c("#FF0000", "#FFFF00", "#017D13"))(100),
    domain = c(0, 100),
    na.color = "grey"
  )
  
  overall_map <- leaflet(india_states_joined) %>%
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
    addLegend(
      pal = pal, 
      values = india_states_joined$Average_Value, 
      title = "Average Value (Bottom 3 Indicators)", 
      labFormat = labelFormat(suffix = "%")
    )
  
  outputs[["overall_map"]] <- overall_map
  
  ### 2. Maps for Each Target (using bottom 3 indicators)
  for (target in unique_targets) {
    target_data <- data %>%
      filter(Target.Number == target) %>%
      # Calculate average for each indicator within each state
      group_by(State, Indicator.Number) %>%
      summarise(Indicator_Avg = mean(Final.Value, na.rm = TRUE), .groups = "drop") %>%
      # Get bottom 3 indicators for each state
      group_by(State) %>%
      arrange(State, Indicator_Avg) %>%
      slice_head(n = 3) %>%
      # Calculate final state average using these bottom 3 indicators
      summarise(Average_Value = mean(Indicator_Avg, na.rm = TRUE))
    
    india_states_joined <- india_states %>%
      left_join(target_data, by = "State")
    
    target_map <- leaflet(india_states_joined) %>%
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
      addLegend(
        pal = pal, 
        values = india_states_joined$Average_Value, 
        title = paste("Average Value for Target", target, "\n(Bottom 3 Indicators)"), 
        labFormat = labelFormat(suffix = "%")
      )
    
    outputs[[paste0("map_target_", target)]] <- target_map
  }
  return(outputs)
}

# Create Static Outputs
static_outputs <- mapcreate(data, india_states, unique_targets)
