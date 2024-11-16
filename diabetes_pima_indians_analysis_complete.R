
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

# Calculate the correlation matrix (excluding the Outcome column if it's binary)
correlation_matrix <- round(cor(diabetes), 1)

# Set column names
colnames(diabetes) <- c("Pregnancies", "Glucose", "Blood Pressure", "Skin Thickness", "Insulin", "Body Mass Index",
                       "Diabetes Pedigree Function", "Age", "Outcome")

# Set consistent row and columnm names
rownames(correlation_matrix) <- colnames(correlation_matrix)

# Create the correlation plot using ggcorrplot
ggcorrplot(
  correlation_matrix,
  hc.order = TRUE,
  type = "full",
  lab = TRUE,
  lab_size = 3,
  colors = viridis::viridis(3)
) +
  ggtitle("Correlation Matrix of Health Factors") +
  theme_minimal()

