library(ggplot2)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(psych)
library(tibble)
library(ggforce)
library(ggdist) 
library(gghalves) 
library(ggridges)
library(scales)
library(corrplot)

# Load the data
data <- read.csv("D:/Hari/Datasets/Custom SDA goal 16/Data_Unpivoted_Custom_SDG16 _LDA.csv")
india_states <- st_read("D:/Hari/Coding/GIS/QGIS/India-State-and-Country/India-State-and-Country/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")

data_distinct <- data %>%
  select(St..Code, State.or.Union.Territory) %>%
  distinct()

# Rename columns to match for merging
data_distinct <- data_distinct %>%
  rename(State = State.or.Union.Territory)

india_states <- india_states %>%
  rename(State = State_Name)

# Ensure the merging column is of the same type
data_distinct$StateName <- as.character(data_distinct$StateName)
india_states$StateName <- as.character(india_states$StateName)


# Ensure 'Score' is a factor with appropriate levels
data$Score <- factor(data$Score, levels = c("red", "orange", "yellow", "green"))

# Compute average subindicator values grouped by state and score
average_subindicator <- subindicator_data %>%
  group_by(`State or Union Territory`, Score) %>%
  summarise(Average = mean(Value, na.rm = TRUE), .groups = 'drop')

# Generate the static map-like bar plot
p1 <- ggplot(average_subindicator, aes(x = reorder(`State or Union Territory`, Average), y = Average, fill = Score)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_fill_manual(values = c("red" = "#D73027", "orange" = "#FC8D59", "yellow" = "#FEE08B", "green" = "#1A9850")) +
  labs(
    title = paste(indicator, '- Average Subindicator Values by State'),
    x = 'State',
    y = 'Average Value'
  ) +
  theme_minimal()

plot(p1)
#ggsave('D:/Hari/Outputs/static_map_average_subindicator.png', plot = p1, width = 12, height = 8)
