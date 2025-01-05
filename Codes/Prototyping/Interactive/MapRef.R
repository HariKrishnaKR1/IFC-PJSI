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

# Function to Create Maps
mapcreate <- function(data, india_states, unique_targets) {
  outputs <- list()
  
  ### 1. Map for Overall Average Score
  overall_data <- data %>%
    group_by(State) %>%
    summarise(Average_Value = mean(Final.Value, na.rm = TRUE))
  
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
    addLegend(pal = pal, values = india_states_joined$Average_Value, title = "Average Value (Overall)", labFormat = labelFormat(suffix = "%"))
  
  outputs[["overall_map"]] <- overall_map
  
  ### 2. Maps for Each Target
  for (target in unique_targets) {
    target_data <- data %>%
      filter(Target.Number == target) %>%
      group_by(State) %>%
      summarise(Average_Value = mean(Final.Value, na.rm = TRUE))
    
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
      addLegend(pal = pal, values = india_states_joined$Average_Value, title = paste("Average Value for Target", target), labFormat = labelFormat(suffix = "%"))
    
    outputs[[paste0("map_target_", target)]] <- target_map
  }

  return(outputs)
}

# Create Static Outputs
static_outputs <- mapcreate(data, india_states, unique_targets)
