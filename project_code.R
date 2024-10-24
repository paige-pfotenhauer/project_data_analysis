#install.packages("pacman")
#library(pacman)
pacman::p_load(tidyverse, 
               devtools,
               janitor,
               dplyr)

#install_github("mvuorre/exampleRPackage")
#help(janitor)

dat <- read.table("GSE203554_normalised.borchelt.final.txt")

#creating a tibble and moving column names to the header
gene_raw <- read.table("GSE203554_normalised.borchelt.final.txt", header = TRUE)

#renaming the headers to change from assigned number to descriptive value

gene_headernames_adjusted <- 
  gene_raw %>% 
   rename(
    WT_1 = WT_191,
    WT_2 = WT_207,
    WT_3 = WT_214,
    WT_4 = WT_218,
    WT_5 = WT_225,
  
    APP_PSI_CNT_1 = APPPI184,
    APP_PSI_CNT_2 = APPPI255,
    APP_PSI_CNT_3 = APPPI262,
    APP_PSI_CNT_4 = APPPI307,
    APP_PSI_CNT_5 = APPPI318,
    
    APP_PSI_BCI838_1 = APPPI198,
    APP_PSI_BCI838_2 =APPPI245,
    APP_PSI_BCI838_3 =APPPI288,
    APP_PSI_BCI838_4 =APPPI322,
    
    APP_PSI_PE_1 = APPPI224,
    APP_PSI_PE_2 = APPPI236,
    APP_PSI_PE_3 = APPPI264,
    APP_PSI_PE_4 = APPPI275,
    APP_PSI_PE_5 = APPPI313,
    
    APP_PSI_BCI838_PE_1 = APPPI182,
    APP_PSI_BCI838_PE_2 = APPPI286,
    APP_PSI_BCI838_PE_3 =APPPI320,
    APP_PSI_BCI838_PE_4 =APPPI321,
    APP_PSI_BCI838_PE_5 =APPPI376
  )
  
