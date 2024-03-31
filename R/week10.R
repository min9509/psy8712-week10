#### Script Settings and Resources ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
install
#### Data Import and Cleaning #### 
# Import SPSS data (Road haven package)
gss_original_tbl <- read_sav(file = "../data/GSS2016.sav")

# Create a variable gss_tbl
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

#### Visualization #### 
ggplot(gss_tbl, aes(x = workhours)) +
  geom_histogram() +
  labs(x = "Working hours",
       y = "Number of data",
       title = "Histogram of working hours")


