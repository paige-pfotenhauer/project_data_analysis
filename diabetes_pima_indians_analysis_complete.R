
#install.packages("pacman")
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
#install.packages("ggcorrplot")
library(ggcorrplot)


# Read data file
diabetes <- read.csv("diabetes.csv")

# Make a copy of the diabetes dataset so the columns can be renamed
diabetes_copy = diabetes

# Set column names
colnames(diabetes_copy) <- c("Pregnancies", "Glucose", "Blood Pressure", "Skin Thickness", "Insulin", "Body Mass Index",
                        "Diabetes Pedigree Function", "Age", "Outcome")

# Calculate the correlation matrix (excluding the Outcome column if it's binary)
correlation_matrix <- round(cor(diabetes_copy), 1)

# Set consistent row and columnm names
rownames(correlation_matrix) <- colnames(correlation_matrix)

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

##############

# Binary logistic regression (Glucose)
set.seed(123)

model <- glm(Outcome ~ Glucose, data = diabetes, family = binomial)
summary(model)

# Create a new data frame for predictions
diabetes$predicted_prob_glucose <- predict(model, type = "response")

ggplot(diabetes, aes(x = Glucose, y = Outcome)) +
geom_point(alpha = 0.5) +  # Scatter plot of actual data
geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
labs(title = "Glucose Levels and Outcome",
   x = "Glucose Level (mg/dL)",
   y = "P(diabetes)") +
theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the plot title
  )

# Model summary and fit statistics
summary(model)

# Convert logistic regression output to odds ratio
exp(coef(model))

# McFadden's R^2
1 - model$deviance / model$null.deviance

##############

# Binary logistic regression (DPF)
set.seed(123)

model <- glm(Outcome ~ DiabetesPedigreeFunction, data = diabetes, family = binomial)
summary(model)

# Create a new data frame for predictions
diabetes$predicted_prob_DPF <- predict(model, type = "response")

ggplot(diabetes, aes(x = DiabetesPedigreeFunction, y = Outcome)) +
  geom_point(alpha = 0.5) +  # Scatter plot of actual data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Diabetes Pedigree Function and Outcome",
       x = "Diabetes Pedigree Function",
       y = "P(diabetes)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the plot title
  )

# Model summary and fit statistics
summary(model)

# Convert logistic regression output to odds ratio
exp(coef(model))

# McFadden's R^2
1 - model$deviance / model$null.deviance

##############

# Binary logistic regression (Glucose, Body Mass Index, Pregnancies, Age)
set.seed(123)

model <- glm(Outcome ~ Glucose + BodyMassIndex + Pregnancies + Age, data = diabetes, family = binomial)
summary(model)

# Create a new data frame for predictions
diabetes$predicted_prob_covariates <- predict(model, type = "response")

ggplot(diabetes, aes(x = , y = Outcome)) +
  geom_point(alpha = 0.5) +  # Scatter plot of actual data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Glucose Levels and Outcome",
       x = "Glucose Level (mg/dL)",
       y = "P(diabetes)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), # Center the plot title
  )

# Model summary and fit statistics
summary(model)

# Convert logistic regression output to odds ratio
exp(coef(model))

# McFadden's R^2
1 - model$deviance / model$null.deviance

##############

# "null model" that's just covariates (sex, age, education)

# "hypothesis model" add in glucose and see how much value it brings to the model with R2