
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
               ggcorrplot,
               )

diabetes <- read.csv("diabetes.csv")

# Correlation matrix of health factors and outcome (diabetes)

data(diabetes)
correlation_plot <- round(cor(diabetes), 1)
head(corr[, 1:8])  

ggcorrplot(correlation_plot, col = viridis)

colnames(diabetes) <- c("Pregnancies", "Glucose", "Blood Pressure", "Skin Thickness", "Insulin", "Body Mass Index",
                           "Diabetes Pedigree Function", "Age", "Outcome")

corr_matrix <- ggcorrplot(
  corr_matrix,
  hc.order = TRUE,
  lab = TRUE, 
  lab_size = 3) +
  guides(fill = guide_legend(title = "Correlation Strength")) +
  ggtitle("Correlation of Diagnostic Health Measurments") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.margin = margin(7, 7, 7, 7),
      legend.justification = "center")
        
# Binary linear regression
#set.seed(123)

#model <- glm(Outcome ~ BMI, data = diabetes, family = binomial)
#summary(model)

# Create a new data frame for predictions
#diabetes$predicted_prob <- predict(model, type = "response")

#ggplot(diabetes, aes(x = BMI, y = Outcome)) +
 # geom_point(alpha = 0.5) +  # Scatter plot of actual data
  #geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  #labs(title = "BMI and Outcome",
   #    x = "BMI",
    #   y = "Outcome") +
  #theme_minimal()

# Model summary and fit statistics
#summary(model)

# McFadden's R^2
#1 - model$deviance / model$null.deviance


############

# Correlation matrix of health factors and outcome (diabetes)
#data(diabetes)
#corr <- round(cor(diabetes), 1)
#head(corr[, 1:8])  

#ggcorrplot(corr)

# Binary linear regression
#set.seed(123)

#model <- glm(Outcome ~ DiabetesPedigreeFunction, data = diabetes, family = binomial)
#summary(model)

# Create a new data frame for predictions
#diabetes$predicted_prob <- predict(model, type = "response")

#ggplot(diabetes, aes(x = DiabetesPedigreeFunction, y = Outcome)) +
 # geom_point(alpha = 0.5) +  # Scatter plot of actual data
  #geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  #labs(title = "BMI and Outcome",
   #    x = "DPF",
    #   y = "Outcome") +
  #theme_minimal()

# Model summary and fit statistics
#summary(model)

# McFadden's R^2
#1 - model$deviance / model$null.deviance



