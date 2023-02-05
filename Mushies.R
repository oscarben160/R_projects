#Loading required packages
library(tidyverse)
library(ggplot2)
library(caret)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
library(e1071)

# data set

mushrooms_clean <- read.csv("~/FALL22/750 - Data Mining & Maching Learning/Project/Mushrooms_Cleaned.csv")

# clean data
mushrooms_clean <- mushrooms_clean[,-1]

mushrooms_clean[sapply(mushrooms_clean, is.character)] <- lapply(mushrooms_clean[sapply(mushrooms_clean, is.character)],  as.factor)

mushrooms_clean$class <- as.numeric(mushrooms_clean$class)

mushrooms_clean["class"][mushrooms_clean["class"]== "1"] <- 0
mushrooms_clean["class"][mushrooms_clean["class"]== "2"] <- 1

mushrooms_clean <- mushrooms_clean[,-17]

mushrooms_clean <- mushrooms_clean[,-6]

#####   view(mushrooms_clean)

# variables choosen by AIC-stepwise
# spore, gill, and ring
# class ~ spore.print.color + gill.size + ring.type

# model
NB<- lm(class ~ spore.print.color + gill.size + ring.type, data = mushrooms_clean)
summary(NB)

  
# random splitting Train and Test data
index<- sample(nrow(mushrooms_clean), nrow(mushrooms_clean)*0.80)
mushroom.train <- mushrooms_clean[index,]
mushroom.test<- mushrooms_clean[-index,]


# Create a navieBayes model
set.seed(1)  # Setting Seed 

classifier_naive <- naiveBayes(class~ spore.print.color + gill.size + ring.type, data = mushroom.train) 

classifier_naive 


# Predicting on test data 

y_pred <- predict(classifier_naive, newdata = mushroom.test) 


### check the accuracry of model

# Confusion Matrix 

conf_mat <- table(mushroom.test$class,y_pred) 

print(conf_mat) 

# Model Evauation 

confusionMatrix(conf_mat) 

?confusionMatrix






