#install.packages("pacman")
#library(pacman)
pacman::p_load(tidyverse, 
               devtools,
               janitor,
               dplyr)

#install_github("mvuorre/exampleRPackage")
#help(janitor)

dat <- read.table("GSE203554_borchelt.final.txt")

#creating a tibble and moving column names to the header
genotyping_tibble <- read.table("GSE203554_borchelt.final.txt", header = TRUE)

#renaming the header names
genotyping_tibble_headernames <- rename(genotyping_tibble, WT_1 = WT_191)

