
install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, 
               devtools,
               janitor,
               dplyr,
               stats,
               patchwork,
               stringr,
               stats,
               patchwork,
               ggplot2,
               tidymodels,
               ggcorrplot
)
install.packages("ggcorrplot")
library(ggcorrplot)


# Read data file
diabetes <- read.csv("diabetes.csv")

# Set column names
colnames(diabetes) <- c("Pregnancies", "Glucose", "Blood Pressure", "Skin Thickness", "Insulin", "Body Mass Index",
                        "Diabetes Pedigree Function", "Age", "Outcome")

# Set consistent row and columnm names
rownames(correlation_matrix) <- colnames(correlation_matrix)

# Calculate the correlation matrix (excluding the Outcome column if it's binary)
correlation_matrix <- round(cor(diabetes), 1)

# Create the correlation plot using ggcorrplot
ggcorrplot(
  correlation_matrix,
  hc.order = TRUE, # Sort correlation matrix hierarchically 
  type = "full",
  lab = TRUE, # Add correlation coefficient labs to plot
  lab_size = 3, # Size of correlation coefficient
  colors = viridis::viridis(3) # Viridis is a color blind friendly color palette
) +
  ggtitle("Correlation Matrix of Health Factors") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels to vertical
    plot.title = element_text(hjust = 0.5), # Center the plot title
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank()   # Remove y-axis title
  ) +
  guides(
    fill = guide_colorbar(title = NULL)  # Remove the color legend title
  )

