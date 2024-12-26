library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)  # For grid layout of multiple plots

# Load the data
data <- read.csv("D:/Hari/Datasets/Custom SDA goal 16/Data_Unpivoted_Custom_SDG16 _LDA.csv")
india_states <- st_read("D:/Hari/Coding/GIS/QGIS/India-State-and-Country/True/India_State_Boundary.shp")

# Ensure the columns are correct
data <- data %>%
  rename(State = State.or.Union.Territory)
data$State <- as.character(data$State)
india_states$State <- as.character(india_states$State)

# Get unique targets
unique_targets <- unique(data$Target.Number)

# Step 1: Plot heatmap with targets (1-9) and average final values for all states
# For each target, compute the mean of "Final.Value" for each state and plot it

target_plots <- list()  # Initialize an empty list to store plots
unique_targets <- unique(data$Target.Number)

# Initialize a list to store maps
map_list <- list()

# Loop through each target to create maps
for (target in unique_targets) {
  # Filter and summarize the data for the current target
  target_data <- data %>%
    filter(Target.Number == target) %>%
    group_by(State) %>%
    summarise(Average_Value = mean(Final.Value, na.rm = TRUE))
  
  # Match the summarized values to the spatial data
  india_states$Average_Value <- target_data$Average_Value[match(india_states$State, target_data$State)]
  
  # Create a static ggplot map for the current target
  map_plot <- ggplot(data = india_states) +
    geom_sf(aes(fill = Average_Value), color = "white", size = 0.2) +
    scale_fill_gradientn(colors = c("lightblue", "blue", "darkblue"), na.value = "grey90", name = "Average Value") +
    labs(title = paste("Target", target, "Average Indicator Value")) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none")
  
  # Append the map to the list
  map_list[[paste0("Target_", target)]] <- map_plot
}

# Arrange all maps in a grid
grid.arrange(
  grobs = map_list,
  ncol = 3,  # Number of columns in the grid
  nrow = ceiling(length(map_list) / 3)  # Number of rows, adjusted dynamically
)

# Step 2: Create a horizontal bar chart of state rankings with average value line

state_ranking_plots <- list()

for (target in unique_targets) {
  
  dt_target <- data %>%
    filter(Target.Number == target) %>%
    group_by(State) %>%
    summarise(Average_Score = mean(Final.Value, na.rm = TRUE)) %>%
    arrange(desc(Average_Score))
  
  average_all_states <- mean(dt_target$Average_Score, na.rm = TRUE)
  
  state_ranking_plots[[paste0("State_Ranking_", target)]] <- ggplot(dt_target, aes(x = reorder(State, Average_Score), y = Average_Score)) +
    geom_col(fill = "steelblue") + 
    geom_hline(yintercept = average_all_states, linetype = "dashed", color = "red") +  # Average line
    coord_flip() +
    labs(title = paste("State Rankings for Target", target), x = "State", y = "Average Score") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
}

# Step 3: Plot values of Indicator Number for each target
indicator_plots <- list()

for (target in unique_targets) {
  
  dt_target <- data %>%
    filter(Target.Number == target) %>%
    group_by(Indicator.Number) %>%
    summarise(Average_Score = mean(Final.Value, na.rm = TRUE))
  
  indicator_plots[[paste0("Indicator_", target)]] <- ggplot(dt_target, aes(x = reorder(Indicator.Number, Average_Score), y = Average_Score)) +
    geom_col(fill = "darkorange") + 
    coord_flip() +
    labs(title = paste("Indicator Scores for Target", target), x = "Indicator Number", y = "Average Score") +
    theme_minimal()
}

# Combine all state ranking plots in a grid (3 rows, 3 columns)
grid.arrange(
  grobs = state_ranking_plots, ncol = 3, nrow = ceiling(length(state_ranking_plots) / 3)
)

# Combine all indicator plots in a grid (3 rows, 3 columns)
grid.arrange(
  grobs = indicator_plots, ncol = 3, nrow = ceiling(length(indicator_plots) / 3)
)
