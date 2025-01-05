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

# Function to Create Static Outputs
create_static_outputs <- function(data, india_states, unique_targets) {
  outputs <- list()
  
  overall_indicator_data <- data %>%
    group_by(Target.Number) %>%
    summarise(
      Average_Score = mean(Final.Value, na.rm = TRUE),
      Description = first(Description)
    )
  
  overall_indicator_chart <- ggplot(overall_indicator_data, aes(x = Average_Score, y = reorder(Target.Number, Average_Score), fill = Average_Score)) +
    geom_col() +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, limits = c(0, 100)) +
    labs(
      title = "Average Scores by Target",
      x = "Average Score",
      y = "Target Number"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  outputs[["overall_indicator_chart"]] <- overall_indicator_chart
  
  ### 4. Bar Charts for Each Target (Filtered by Target)
  for (target in unique_targets) {
    target_indicator_data <- data %>%
      filter(Target.Number == target) %>%
      group_by(Indicator.Number) %>%
      summarise(
        Average_Score = mean(Final.Value, na.rm = TRUE),
        Description = first(Description)
      )
    
    target_chart <- ggplot(target_indicator_data, aes(x = Average_Score, y = reorder(Indicator.Number, Average_Score), fill = Average_Score)) +
      geom_col() +
      scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, limits = c(0, 100)) +
      labs(
        title = paste("Indicator Scores for Target", target),
        x = "Average Score",
        y = "Indicator Number"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    outputs[[paste0("bar_chart_target_", target)]] <- target_chart
  }
  
  ### 5. Table: State-wise Scores for Each Target and Overall
  for (target in c(NA, unique_targets)) {
    if (is.na(target)) {
      state_data <- data %>%
        group_by(State) %>%
        summarise(Average_Score = round(mean(Final.Value, na.rm = TRUE), 2))
      outputs[["st_table_ovr"]] <- state_data
    } else {
      state_data <- data %>%
        filter(Target.Number == target) %>%
        group_by(State) %>%
        summarise(Average_Score = round(mean(Final.Value, na.rm = TRUE), 2))
      outputs[[paste0("st_table_t_", target)]] <- state_data
    }
  }
  
  ### 6. Table: Target Descriptions and Scores
  target_table <- data %>%
    group_by(Target.Number) %>%
    summarise(
      Description = first(Description),
      Average_Score = round(mean(Final.Value, na.rm = TRUE), 2)
    )
  outputs[["target_table_ovr"]] <- target_table
  
  ### 7. Tables for Each Target: Indicator Descriptions and Scores
  for (target in unique_targets) {
    indicator_data <- data %>%
      filter(Target.Number == target) %>%
      group_by(Indicator.Number) %>%
      summarise(
        Description = first(Description),
        Average_Score = round(mean(Final.Value, na.rm = TRUE), 2)
      )
    outputs[[paste0("target_table_t", target)]] <- indicator_data
  }
  
  ### 8. Heatmap for All Targets (Overall View)
  heatmap_data <- data %>%
    group_by(State, Target.Number) %>%
    summarise(
      Average_Value = round(mean(Final.Value, na.rm = TRUE), 2),
      Description = first(Description),
      State_Code = first(St..Code),
      .groups = "drop"
    ) %>%
    complete(State, Target.Number, fill = list(Average_Value = NA, Description = "NA")) %>%
    mutate(Negative_State_Code = -as.numeric(State_Code))  # Create negative state code for ordering
  
  heatmap_overall <- ggplot(heatmap_data, aes(x = as.factor(Target.Number), y = reorder(State, Negative_State_Code), fill = Average_Value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(!is.na(Average_Value), Average_Value, "")), size = 3, color = "black", fontface = "bold") +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, na.value = "grey") +
    labs(
      title = "Average Values Across States and Targets",
      x = "Target Number",
      y = "",
      fill = "Avg Value"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  outputs[["heatmap_overall"]] <- heatmap_overall
  
  
  # State-wise Bar Charts
  for (target in c(NA, unique_targets)) {
    if (is.na(target)) {
      state_data <- data %>%
        group_by(State) %>%
        summarise(Average_Score = round(mean(Final.Value, na.rm = TRUE), 2))
      
      state_chart <- ggplot(state_data, aes(x = Average_Score, y = reorder(State, Average_Score), fill = Average_Score)) +
        geom_col() +
        scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, limits = c(0, 100)) +
        labs(
          title = "State-wise Average Scores (Overall)",
          x = "Average Score",
          y = "State"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
      outputs[["state_chart_overall"]] <- state_chart
    } else {
      state_data <- data %>%
        filter(Target.Number == target) %>%
        group_by(State) %>%
        summarise(Average_Score = round(mean(Final.Value, na.rm = TRUE), 2))
      
      state_chart <- ggplot(state_data, aes(x = Average_Score, y = reorder(State, Average_Score), fill = Average_Score)) +
        geom_col() +
        scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", midpoint = 50, limits = c(0, 100)) +
        labs(
          title = paste("State-wise Average Scores for Target", target),
          x = "Average Score",
          y = "State"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
      outputs[[paste0("state_chart_target_", target)]] <- state_chart
    }
  }
  
  return(outputs)
}

# Create Static Outputs
static_outputs <- create_static_outputs(data, india_states, unique_targets)


# Save all ggplot outputs
output_directory <- "directory here"  # Specify your output directory
# Iterate through the outputs and save the ggplot objects
for (name in names(static_outputs)) {
  plot_object <- static_outputs[[name]]
  
  if (inherits(plot_object, "ggplot")) {  # Check if the object is a ggplot
    file_name <- paste0(output_directory, "/", name, ".png")  # Construct the file path
    ggsave(
      filename = file_name,
      plot = plot_object,
      width = 10,  # Adjust width
      height = 6,  # Adjust height
      dpi = 300    # High resolution
    )
    message(paste("Saved:", file_name))
  }
}
