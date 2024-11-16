```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

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
               tidymodels)

raw_tree_Data <- read.csv("forest_health_data_with_target.csv")

# Clean the data by removing rows with NA values
tree_data <- na.omit(raw_tree_Data)

# Predict tree height using humidity
lin_reg_model <- linear_reg() %>% 
  set_engine("lm") %>%
  fit(Tree_Height ~ Humidity, data = tree_data)

tidy(lin_reg_model)

ggplot(data = tree_data, aes(x = Soil_TP, Crown_Width_North_South)) +
  geom_point() +
  geom_smooth(method = "lm")
