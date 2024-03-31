#### Script Settings and Resources ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
set.seed(8712)

#### Data Import and Cleaning #### 
# Import SPSS data (Road haven package)
gss_original_tbl <- read_sav(file = "../data/GSS2016.sav")

# Create a variable gss_tbl
gss_tbl <- gss_original_tbl %>%
  # Remove rows where MOSTHRS is missing by using filter and !is.na
  filter(!is.na(MOSTHRS)) %>%
  # Rename MOSTHRS to workhours by using rename
  rename(workhours = MOSTHRS) %>%
  # Remove HRS1 and HRS2 variables by using select 
  select(-c(HRS1, HRS2)) %>%
  # Retain only variables with less than 75% missingness by using select
  select(where(~mean(is.na(.)) < 0.75)) %>%
  # Convert to data to numeric variables
  sapply(as.numeric) %>% 
  # Convert to data frame to ensure compatibility with dplyr functions by using as.data.frame
  as_tibble()  

#### Visualization #### 
# Making the histogram
ggplot(gss_tbl, aes(x = workhours)) +
  geom_histogram() +
  labs(x = "Working hours",
       y = "Number of data",
       title = "Histogram of working hours")

#### Analysis #### 
## Prepare data set to run ML
# Create sample rows  
gss_sample <- sample(nrow(gss_tbl))
# Shuffle using the sampled indices 
gss_shuffle <- gss_tbl[gss_sample,]
# Calculate the number of rows for a 75/25 split
gss_75per <- round(nrow(gss_shuffle) * .75)
# Create the training set using the first 75% of the shuffled data
gss_train <- gss_shuffle[1:gss_75per,]
# Create the test set using the remaining 25% of the shuffled data
gss_test <- gss_shuffle[(gss_75per + 1):nrow(gss_shuffle),]
# Create 10 folds for cross-validation using the workhours column from the training set
gss_folds <- createFolds(gss_train$workhours, 10)
# Set up train control for all model
train_control <- trainControl(method = "cv", 
                              number = 10, 
                              index = gss_folds, 
                              verboseIter = TRUE)

## Run OLS,  elastic net, random forest, and eXtreme Gradient Boosting
# Train the OLS regression model using train
model_OLS <- train(
  workhours ~ .,
  data = gss_train,
  method = "lm", 
  metric = "Rsquared",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = train_control
)

# Train the elastic net model using train
model_elastic <- train(
  workhours ~ .,
  data = gss_train,
  method = "glmnet", 
  metric = "Rsquared",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = train_control
)

# Train the random forest model using train
model_random <- train(
  workhours ~ .,
  data = gss_train,
  method = "ranger", 
  metric = "Rsquared",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = train_control
)

# Train the random XGB using train
model_random <- train(
  workhours ~ .,
  data = gss_train,
  method = "xgbLinear", 
  metric = "Rsquared",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = train_control
)



