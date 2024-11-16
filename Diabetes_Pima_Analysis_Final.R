knitr::opts_chunk$set(echo = TRUE)

install.packages("pacman")
library(pacman,
        viridis)
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
               ggcorrplot)

diabetes <- read.csv("diabetes.csv")

# Correlation matrix of health factors and outcome (diabetes)
data(diabetes)
correlation_plot <- round(cor(diabetes), 1)
head(corr[, 1:8])  

ggcorrplot(correlation_plot, col = viridis)
