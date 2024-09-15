# Load necessary libraries
library(plotly)
library(RColorBrewer)

# Sample Data for Wicket Types
wicket_data <- data.frame(
  WicketType = c("Bowled", "Caught", "LBW", "Stumped", "Run Out"),
  Count = c(30, 45, 15, 10, 5)
)

# Automatically generate a color palette based on the number of unique wicket types
num_wicket_types <- length(unique(wicket_data$WicketType))
colors <- brewer.pal(min(num_wicket_types, 9), "Set3")  # Use 'Set3' palette and cap it at 9 colors (max for this palette)

# Create the pie chart
plot_ly(
  data = wicket_data, 
  labels = ~WicketType, 
  values = ~Count, 
  type = 'pie', 
  textinfo = 'label+percent',    # Show both label and percentage
  insidetextorientation = 'radial', # Text orientation inside pie slices
  marker = list(colors = colors)  # Use auto-generated colors
) %>%
  layout(
    title = "Wicket Distribution by Type", # Title of the chart
    showlegend = TRUE                      # Display legend
  )
