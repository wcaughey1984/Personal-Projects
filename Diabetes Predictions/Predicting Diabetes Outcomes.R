# 
# Title:    Predicting Diabetes Outcomes
# Purpose:  (nThrive) Determine best model for predicting diabetic outcome
# Author:   Billy Caughey
# Date:     2020.12.27 - Initial Build 
#

##### Libraries #####

library(MASS)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

##### Import Data #####

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00529/diabetes_data_upload.csv"
diab_data <- read.csv(url) %>%
    rename(Outcome = class) %>%
    mutate(age_cat = (`Age` < 50),
           Outcome = (Outcome == "Positive"))

##### Train / Test Sets #####

set.seed(1234)

trainIDs <- createDataPartition(y = diab_data$`Outcome`,
                                p = 0.7,
                                list = FALSE,
                                times = 1)

train_set <- diab_data[trainIDs,] 
test_set <- diab_data[-trainIDs,] 

##### Logistic Regression #####

## Train Model

mod1 <- glm(`Outcome` ~ . - Age, 
            data = train_set,
            family = binomial(link = "logit"))

summary(mod1)

## Test Model

predictions_mod1 <- predict(object = mod1,
                            newdata = test_set, 
                            type = "response")

test_results_logit <- tibble(
    `Actual Outcome` = test_set$Outcome,
    `Results` = predictions_mod1
) %>%
    mutate(`Predicted Outcome` = factor((`Results` > 0.5)),
           `Actual Outcome` = factor(`Actual Outcome`))

## Test Model

tab1 <- with(test_results_logit, table(`Actual Outcome`, `Predicted Outcome`))
(tab1[1,1] + tab1[2,2]) / sum(tab1)
with(test_results_logit, sensitivity(`Actual Outcome`, `Predicted Outcome`))
with(test_results_logit, specificity(`Actual Outcome`, `Predicted Outcome`))

##### Linear Discriminate Analysis #####

## Train Model

mod2 <- lda(`Outcome` ~ . - Age, data = train_set, prior = c(1-0.105, 0.105))
summary(mod2)

predictions_mod2 <- mod2 %>% predict(test_set)
names(predictions_mod2)

## Test Model

test_results_lda <- tibble(
    `Actual Outcome` = test_set$Outcome,
    `Predicted Outcome` = predictions_mod2$class
) %>%
    mutate(`Predicted Outcome` = factor(`Predicted Outcome`),
           `Actual Outcome` = factor(`Actual Outcome`))

tab2 <- with(test_results_lda, table(`Actual Outcome`, `Predicted Outcome`))
(tab2[1,1] + tab2[2,2]) / sum(tab2)
with(test_results_lda, sensitivity(`Actual Outcome`, `Predicted Outcome`))
with(test_results_lda, specificity(`Actual Outcome`, `Predicted Outcome`))

##### Classification Tree #####

## Train Model

mod3 <- rpart(`Outcome` ~ . - Age, data = train_set, method = "class")
summary(mod3)
prp(mod3)

## Test Model

prediction_mod3 <- mod3 %>% predict(test_set)

test_results_ct <- tibble(
    `Actual Outcome` = test_set$Outcome,
    `Predicted Outcome` = (prediction_mod3[,2] > prediction_mod3[,1])
) %>%
    mutate(`Predicted Outcome` = factor(`Predicted Outcome`),
           `Actual Outcome` = factor(`Actual Outcome`))

tab3 <- with(test_results_ct, table(`Actual Outcome`, `Predicted Outcome`))
(tab3[1,1] + tab3[2,2]) / sum(tab3)
with(test_results_ct, sensitivity(`Actual Outcome`, `Predicted Outcome`))
with(test_results_ct, specificity(`Actual Outcome`, `Predicted Outcome`))

##### Random Forest #####

## Train Model

mod4 <- randomForest(as.factor(`Outcome`) ~ . - Age, 
                     data = train_set,
                     ntree = 500,
                     importance = TRUE,
                     nodesize = 5)

varImpPlot(mod4, type = 1)

## Test Model

prediction_mod4 <- mod4 %>% predict(test_set)

test_results_rf <- tibble(
    `Actual Outcome` = test_set$Outcome,
    `Predicted Outcome` = prediction_mod4
) %>%
    mutate(`Predicted Outcome` = factor(`Predicted Outcome`),
           `Actual Outcome` = factor(`Actual Outcome`))

tab4 <- with(test_results_rf, table(`Actual Outcome`, `Predicted Outcome`))
(tab4[1,1] + tab4[2,2]) / sum(tab4)
with(test_results_rf, sensitivity(`Actual Outcome`, `Predicted Outcome`))
with(test_results_rf, specificity(`Actual Outcome`, `Predicted Outcome`))















