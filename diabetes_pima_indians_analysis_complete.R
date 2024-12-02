
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
               kableExtra,
               broom
)
#install.packages("ggcorrplot")
library(ggcorrplot)


# Read data file
diabetes <- read.csv("diabetes.csv")

################
# Data Cleaning
################

# Make a copy of the dataset for cleaning
diabetes_clean = diabetes

# Set column names
colnames(diabetes_clean) <- c("Pregnancies", "Glucose", "Blood_Pressure", "Skin_Thickness", "Insulin", "BMI",
                             "Diabetes_Pedigree_Function", "Age", "Outcome")

# Calculate the mean of the columns with illogical zero values, excluding zeros
glucose_mean <- mean(diabetes_clean$Glucose[diabetes_clean$Glucose != 0])
bloodpressure_mean <- mean(diabetes_clean$Blood_Pressure[diabetes_clean$Blood_Pressure != 0])
skinthickness_mean <- mean(diabetes_clean$Skin_Thickness[diabetes_clean$Skin_Thickness != 0])
insulin_mean <- mean(diabetes_clean$Insulin[diabetes_clean$Insulin != 0])
BMI_mean <- mean(diabetes_clean$BMI[diabetes_clean$BMI != 0])

# Replace zero values in the columns with illogical zero values with the calculated mean
diabetes_clean$Glucose[diabetes_clean$Glucose == 0] <- glucose_mean
diabetes_clean$Blood_Pressure[diabetes_clean$Blood_Pressure == 0] <- bloodpressure_mean
diabetes_clean$Skin_Thickness[diabetes_clean$Skin_Thickness == 0] <- skinthickness_mean
diabetes_clean$Insulin[diabetes_clean$Insulin== 0] <- insulin_mean
diabetes_clean$BMI[diabetes_clean$BMI== 0] <- BMI_mean

#####################
# Correlation Matrix
#####################

# Make a copy of the diabetes data set so the columns can be renamed for correlation matrix
diabetes_copy = diabetes_clean

# Set column names
colnames(diabetes_copy) <- c("Pregnancies", "Glucose", "Blood Pressure", "Skin Thickness", "Insulin", "Body Mass Index",
                        "Diabetes Pedigree Function", "Age", "Outcome")

# Calculate the correlation matrix
correlation_matrix <- round(cor(diabetes_copy), 1)

# Set consistent row and column names
rownames(correlation_matrix) <- colnames(correlation_matrix)

# Create the correlation plot using ggcorrplot
ggcorrplot(
  correlation_matrix,
  #hc.order = TRUE, # Sort correlation matrix hierarchically 
  type = "full",
  lab = TRUE, # Add correlation coefficient labels to plot
  lab_size = 3, # Size of correlation coefficient
  colors = viridis::viridis(3) # Viridis is a color blind friendly color palette
) +
  ggtitle("Correlation Matrix of Health Metrics and Diabetes") +
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

#######################################
# Binary logistic regression (Glucose)
#######################################

# Set seed for reproducible results each time running code
set.seed(123)

# Binary logistic regression
model_glucose <- glm(Outcome ~ Glucose, data = diabetes_clean, family = binomial)
summary(model_glucose)

# Create a new data frame for predictions
diabetes_clean$predicted_prob_glucose <- predict(model_glucose, type = "response")

ggplot(diabetes_clean, aes(x = Glucose, y = Outcome)) +
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
summary(model_glucose)

# Convert logistic regression output to odds ratio
exp(coef(model_glucose))

# McFadden's R^2
1 - model_glucose$deviance / model_glucose$null.deviance

############################################
# Binary logistic regression (Glucose + BMI)
############################################

set.seed(123)

model_glucose_BMI <- glm(Outcome ~ Glucose + BMI, data = diabetes_clean, family = binomial)
summary(model_glucose_BMI)

# Model summary and fit statistics
summary(model_glucose_BMI)

# Convert logistic regression output to odds ratio
exp(coef(model_glucose_BMI))

# McFadden's R^2
1 - model_glucose_BMI$deviance / model_glucose_BMI$null.deviance

#################################################################
# Produce a summary statistics table for key variable model
#################################################################

# Summary statistics
summary_stats_model_key <- summary(model_glucose_BMI)

# Convert logistic regression output to odds ratio
odds_ratios_model_key <- exp(coef(model_glucose_BMI))

# Extract metrics
results_model_key <- tidy(model_glucose_BMI)

# Calculate Chi-Square values
results_model_key <- results_model_key %>%
  mutate(chi_square = (estimate / std.error)^2,
         odds_ratio = exp(estimate))

# Remove underscores from table names
table_data_model_key$Variable <- gsub("_", " ", table_data_model_key$Variable)

# Generate table
kable(
  table_data_model_key,
  col.names = c("Variable", "Coefficient", "Chi Square", "P-Value", "Odds Ratio"),
  format = "html",
  digits = 5,
  caption = "Full Model (All Variables) Logistic Regression Summary"
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


############################################
# Binary logistic regression (All Variables)
############################################

set.seed(123)

model_all <- glm(Outcome ~ Glucose + Diabetes_Pedigree_Function + Pregnancies + Glucose + Blood_Pressure + Skin_Thickness + Insulin + BMI + Age , data = diabetes_clean, family = binomial)
summary(model_all)

# Create a new data frame for predictions
diabetes_clean$predicted_prob_AllVariables <- predict(model_all, type = "response")

# Model summary and fit statistics
summary(model_all)

# Convert logistic regression output to odds ratio
exp(coef(model_all))

# McFadden's R^2
1 - model_all$deviance / model_all$null.deviance


#################################################################
# Produce a summary statistics table for the all variable model
#################################################################

# Summary statistics
summary_stats_model_all <- summary(model_all)

# Convert logistic regression output to odds ratio
odds_ratios_model_all <- exp(coef(model_all))

# Extract metrics
results_model_all <- tidy(model_all)

# Calculate Chi-Square values
results_model_all <- results_model_all %>%
  mutate(chi_square = (estimate / std.error)^2,
         odds_ratio = exp(estimate))

# Remove underscores from table names
table_data_model_all$Variable <- gsub("_", " ", table_data_model_all$Variable)

# Generate table
kable(
  table_data_model_all,
  col.names = c("Variable", "Coefficient", "Chi Square", "P-Value", "Odds Ratio"),
  format = "html",
  digits = 5,
  caption = "Full Model (All Variables) Logistic Regression Summary"
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


################################################################################################
# Format a table of logistic regression results (glucose vs glucose + insulin vs all variables)
################################################################################################

# Custom model names
custom_model_names <- c("Single Predictor (Glucose)", "Key Variables (Glucose + BMI)", "Full (All Variables)")

# Calculate AIC and McFadden's R^2 for each model
model_comparison <- data.frame(
  Model = custom_model_names,
  AIC = c(AIC(model_glucose), AIC(model_glucose_BMI), AIC(model_all)),
  McFaddens_R2 = c(
    1 - model_glucose$deviance / model_glucose$null.deviance,
    1 - model_glucose_BMI$deviance / model_glucose_BMI$null.deviance,
    1 - model_all$deviance / model_all$null.deviance
  )
)

# Comparison table using Kable
model_comparison %>%
  kable(
    caption = "Comparison of Logistic Regression Models",
    col.names = c("Model", "AIC", "McFadden's R^2"),
    digits = 3,  # Round numbers to 3 decimal places
    align = c("l", "l", "l"),
    format = "html"
  ) %>%
  kable_styling(bootstrap_options = c("striped"), full_width = F)

