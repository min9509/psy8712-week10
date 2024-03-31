#### Script Settings and Resources ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

#### Data Import and Cleaning #### 

gss_original_tbl <- read_sav(file = "../data/GSS2016.sav")

gss_tbl <- gss_original_tbl %>%
  # Convert to data frame to ensure compatibility with dplyr functions by using as.data.frame
  as.data.frame() %>%  
  # Remove rows where MOSTHRS is missing by using filter and !is.na
  filter(!is.na(MOSTHRS)) %>%
  # Rename MOSTHRS to workhours by using rename
  rename(workhours = MOSTHRS) %>%
  # Remove HRS1 and HRS2 variables by using select 
  select(-c(HRS1, HRS2)) %>%
  # Retain only variables with less than 75% missingness by using select
  select(where(~mean(is.na(.)) < 0.75))



