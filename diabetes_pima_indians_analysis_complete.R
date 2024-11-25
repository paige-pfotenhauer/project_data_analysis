
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
               ggcorrplot,
               knitr,
               pROC,
               kableExtra
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
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center the plot title
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank()   # Remove y-axis title
  ) +
  guides(
    fill = guide_colorbar(title = NULL)  # Remove the color legend title
  )

##############
##############

# Binary logistic regression (Glucose)
set.seed(123)

model_glucose <- glm(Outcome ~ Glucose, data = diabetes, family = binomial)
summary(model)

# Create a new data frame for predictions
diabetes$predicted_prob_glucose <- predict(model, type = "response")

ggplot(diabetes, aes(x = Glucose, y = Outcome)) +
  geom_point(alpha = 0.5) +  # Scatter plot of actual data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "red", fill = "pink", linewidth = 1.2) +
    labs(
      title = "Glucose Levels and Outcome",
      x = "Glucose Level (mg/dL)",
      y = "P(diabetes)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # Black border
  )

# Model summary and fit statistics
summary(model)

# Convert logistic regression output to odds ratio
exp(coef(model))

# McFadden's R^2
1 - model$deviance / model$null.deviance

##############
##############

# Binary logistic regression (Glucose + BMI)
set.seed(123)

model_glucose_insulin <- glm(Outcome ~ Glucose + BMI, data = diabetes, family = binomial)
summary(model)

# Model summary and fit statistics
summary(model)

# Convert logistic regression output to odds ratio
exp(coef(model))

# McFadden's R^2
1 - model$deviance / model$null.deviance


##############
##############

# Binary logistic regression (All Variables)
set.seed(123)

model_all <- glm(Outcome ~ Glucose + DiabetesPedigreeFunction + Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + Age , data = diabetes, family = binomial)
summary(model)

# Create a new data frame for predictions
diabetes$predicted_prob_AllVariables <- predict(model, type = "response")

# Model summary and fit statistics
summary(model)

# Convert logistic regression output to odds ratio
exp(coef(model))

# McFadden's R^2
1 - model$deviance / model$null.deviance

##############
##############

# Format a table of logistic regression results (glucose vs glucose + insulin vs all variables)

# Custom model names
custom_model_names <- c("Single Predictor (Glucose)", "Reduced (Glucose + BMI)", "Full (All Variables)")

# Calculate AIC and McFadden's R^2 for each model
model_comparison <- data.frame(
  Model = custom_model_names,
  AIC = c(AIC(model_glucose), AIC(model_glucose_insulin), AIC(model_all)),
  McFaddens_R2 = c(
    1 - model_glucose$deviance / model_glucose$null.deviance,
    1 - model_glucose_insulin$deviance / model_glucose_insulin$null.deviance,
    1 - model_all$deviance / model_all$null.deviance
  )
)

# Comparison table using Kable
model_comparison %>%
  kable(
    caption = "Table 1: Comparison of Logistic Regression Models",
    col.names = c("Model", "AIC", "McFadden's R^2"),
    digits = 3,  # Round numbers to 3 decimal places
    align = c("l", "l", "l"),
    format = "html"
  ) %>%
  kable_styling(bootstrap_options = c("striped"), full_width = F)

