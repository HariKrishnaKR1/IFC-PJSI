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

create_static_outputs <- function(data, india_states, unique_targets) {
  outputs <- list()
  
  # Calculate overall indicator averages using bottom 3 indicators for each target
  overall_indicator_data <- data %>%
    group_by(Target.Number, Indicator.Number) %>%
    summarise(
      Indicator_Avg = mean(Final.Value, na.rm = TRUE),
      Description = first(Description)
    ) %>%
    group_by(Target.Number) %>%
    arrange(Target.Number, Indicator_Avg) %>%
    slice_head(n = 3) %>%  # Take lowest 3 indicators
    summarise(
      Average_Score = mean(Indicator_Avg, na.rm = TRUE),
      Description = first(Description)
    )
  
  overall_indicator_chart <- ggplot(overall_indicator_data, 
                                    aes(x = Average_Score, y = reorder(Target.Number, Average_Score), fill = Average_Score)) +
    geom_col() +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", 
                         midpoint = 50, limits = c(0, 100)) +
    labs(
      title = "Average Scores by Target (Bottom 3 Indicators)",
      x = "Average Score",
      y = "Target Number"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  outputs[["overall_indicator_chart"]] <- overall_indicator_chart
  
  # Bar Charts for Each Target (Bottom 3 Indicators)
  for (target in unique_targets) {
    target_indicator_data <- data %>%
      filter(Target.Number == target) %>%
      group_by(Indicator.Number) %>%
      summarise(
        Average_Score = mean(Final.Value, na.rm = TRUE),
        Description = first(Description)
      ) %>%
      arrange(Average_Score) %>%
      slice_head(n = 3)  # Take lowest 3 indicators
    
    target_chart <- ggplot(target_indicator_data, 
                           aes(x = Average_Score, y = reorder(Indicator.Number, Average_Score), 
                               fill = Average_Score)) +
      geom_col() +
      scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", 
                           midpoint = 50, limits = c(0, 100)) +
      labs(
        title = paste("Bottom 3 Indicator Scores for Target", target),
        x = "Average Score",
        y = "Indicator Number"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    outputs[[paste0("bar_chart_target_", target)]] <- target_chart
  }
  
  # State-wise Scores (using bottom 3 indicators)
  for (target in c(NA, unique_targets)) {
    if (is.na(target)) {
      # Overall state averages using bottom 3 indicators for each target
      state_data <- data %>%
        group_by(Target.Number, Indicator.Number, State) %>%
        summarise(Indicator_Avg = mean(Final.Value, na.rm = TRUE), .groups = "drop") %>%
        group_by(Target.Number, State) %>%
        arrange(Target.Number, Indicator_Avg) %>%
        slice_head(n = 3) %>%
        group_by(State) %>%
        summarise(Average_Score = round(mean(Indicator_Avg, na.rm = TRUE), 2))
      
      outputs[["st_table_ovr"]] <- state_data
    } else {
      # Target-specific state averages using bottom 3 indicators
      state_data <- data %>%
        filter(Target.Number == target) %>%
        group_by(State, Indicator.Number) %>%
        summarise(Indicator_Avg = mean(Final.Value, na.rm = TRUE), .groups = "drop") %>%
        group_by(State) %>%
        arrange(Statdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAkCAYAAAD7PHgWAAABBklEQVR4Xu2XMQrCQBBFBQvR6wgJHsEDpHVjBDvvoBhbI3bWCkZbFUyhFrYiEat0WgmC6AVkdQqbIVmWZAOi82C64b+/bDWZDEEQP4phTLMaa9d003bTGMgu1psF7JVGNzuWPdzs18GDz443rgrIcndXbvW8g1axGfZKo7P2eBXc+WB74a3FGXtiA1kwzfnpqTF7hL3SwDfAaz+BqvjkwYADe6WhglQwJlQwKVQwKakVTGOoYNL5z4JxwBlUMEwqAu9SwTCpCLxLBcOkIvCusoKT9/WFQ6OkIvCukoJwt5rO0sehUVIReBem6ng+OLBXmnKjn4PbGM5PeKnqgXIlo5vHXoL4Nl4ZYqbbEGA7+wAAAABJRU5ErkJggg==e, Indicator_Avg) %>%
        slice_head(n = 3) %>%
        summarise(Average_Score = round(mean(Indicator_Avg, na.rm = TRUE), 2))
      
      outputs[[paste0("st_table_t_", target)]] <- state_data
    }
  }
  
  # Heatmap with bottom 3 indicators average
  heatmap_data <- data %>%
    group_by(Target.Number, Indicator.Number, State) %>%
    summarise(
      Indicator_Avg = mean(Final.Value, na.rm = TRUE),
      State_Code = first(St..Code),
      Description = first(Description),
      .groups = "drop"
    ) %>%
    group_by(Target.Number, State) %>%
    arrange(Target.Number, State, Indicator_Avg) %>%
    slice_head(n = 3) %>%
    summarise(
      Average_Value = round(mean(Indicator_Avg, na.rm = TRUE), 2),
      State_Code = first(State_Code),
      Description = first(Description),
      .groups = "drop"
    ) %>%
    mutate(Negative_State_Code = -as.numeric(State_Code))
  
  heatmap_overall <- ggplot(heatmap_data, 
                            aes(x = as.factor(Target.Number), y = reorder(State, Negative_State_Code), 
                                fill = Average_Value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Average_Value), size = 3, color = "black", fontface = "bold") +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#017D13", 
                         midpoint = 50, na.value = "grey") +
    labs(
      title = "Average Values Across States and Targets (Bottom 3 Indicators)",
      x = "Target Number",
      y = "",
      fill = "Avg Value"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  outputs[["heatmap_overall"]] <- heatmap_overall
  
  return(outputs)
}

# Create Static Outputs
static_outputs <- create_static_outputs(data, india_states, unique_targets)


# Save all ggplot outputs
output_directory <- "D:/Hari/Coding/R/Custom SDG 16/Interactive/"  # Specify your output directory
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
