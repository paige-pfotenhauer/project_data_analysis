#install.packages("pacman")
#library(pacman)
pacman::p_load(tidyverse, 
               devtools,
               janitor,
               dplyr,
               stats,
               patchwork,
               stringr,
               stats,
               patchwork,
               ggplot2)

#install_github("mvuorre/exampleRPackage")
#help(janitor)

athlete_data <- read.csv("forbes_richest_athletes_1990_2020.csv")

# Cleaning data to have uniform capitalization and combined equivalent subtypes
clean_athlete_data <- athlete_data %>%
  mutate(
    Sport = str_to_title(Sport),  # Capitalize first letters of each word
    Sport = ifelse(Sport == "Nfl", "American Football", Sport),
    Sport = ifelse(Sport == "Nascar", "Auto Racing", Sport),
    Sport = ifelse(Sport == "Auto Racing (Nascar)", "Auto Racing", Sport),
    Sport = ifelse(Sport == "F1 Racing", "Auto Racing", Sport),
    Sport = ifelse(Sport == "F1 Motorsports", "Auto Racing", Sport),
    Sport = ifelse(Sport == "Nba", "Basketball", Sport),
    Sport = ifelse(Sport == "Hockey", "Ice Hockey", Sport),
    Sport = ifelse(Sport == "Motorcycle Gp", "Motorcycle GP", Sport),
    Sport = ifelse(Sport == "Mma", "Professional Fighting", Sport),
    Sport = ifelse(Sport == "Boxing", "Professional Fighting", Sport)
  )

# Bar plot for count of athletes per nationality
ggplot(clean_athlete_data, aes(x = Nationality)) +
  geom_bar() +
  labs(title = "Count of Athletes by Nationality",
       x = "Nationality",
       y = "Count") +
  theme_minimal()

# Bar plot for count of athletes per sport
ggplot(clean_athlete_data, aes(x = Sport)) +
  geom_bar() +
  labs(title = "Count of Athletes by Sport",
       x = "Sport",
       y = "Count") +
  theme_minimal() +
  guides(x = guide_axis(angle = 45))



