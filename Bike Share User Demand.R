## Clearing the Environment and Console
rm(list = ls())
cat('\014')

## Loading Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(caret)
library(corrplot)
library(reshape2)

## Reading the Data
bike <- read.csv("Bicycle.csv")

## Data Pre-processing
# Checking the head of the dataset
head(bike)

# Checking the size of the dataset
dim(bike)

# Viewing descriptive information
str(bike)
summary(bike)

# Checking for missing values
round(colMeans(is.na(bike)) * 100, 2)
round(rowMeans(is.na(bike)) * 100, 2)

# Checking for duplicates and removing them
bike <- bike[!duplicated(bike), ]
dim(bike)

# Creating a copy of the dataframe without the 'instant', 'yr', and 'dteday' columns
bike_dummy <- bike[, -c(which(names(bike) %in% c("instant", "yr", "dteday")))]

# Checking the datatypes before conversion
str(bike_dummy)


## Splitting the data into training and testing datasets
set.seed(123)
index <- createDataPartition(bike_dummy$cnt, p = 0.8, list = FALSE)
df_train <- bike_dummy[index, ]
df_test <- bike_dummy[-index, ]

## Exploratory Data Analysis
# Creating a new dataframe of only numeric variables
bike_num <- df_train[, c('temp', 'atemp', 'hum', 'windspeed', 'cnt')]

# Plotting pairplot of numeric variables
pairs(~temp + atemp + hum + windspeed + cnt, data = bike_num)

# Building boxplot of all categorical variables against the target variable 'cnt'
par(mfrow=c(2, 3))
boxplot(cnt ~ season, data = bike_dummy)
boxplot(cnt ~ mnth, data = bike_dummy)
boxplot(cnt ~ weathersit, data = bike_dummy)
boxplot(cnt ~ holiday, data = bike_dummy)
boxplot(cnt ~ weekday, data = bike_dummy)
boxplot(cnt ~ workingday, data = bike_dummy)

## Correlation Analysis
# Checking the correlation coefficients
correlation_matrix <- cor(bike_dummy)
corrplot(correlation_matrix, method = "circle")

## Scale Numeric Variables
scaler <- preProcess(df_train[, c('temp', 'atemp', 'hum', 'windspeed')], method = c("center", "scale"))
df_train[, c('temp', 'atemp', 'hum', 'windspeed')] <- predict(scaler, df_train[, c('temp', 'atemp', 'hum', 'windspeed')])
df_test[, c('temp', 'atemp', 'hum', 'windspeed')] <- predict(scaler, df_test[, c('temp', 'atemp', 'hum', 'windspeed')])

## Linear Regression Model for Total Users
# Building a Linear Regression Model for Total Users
lm_model_cnt <- lm(cnt ~ season + mnth + holiday + weekday + workingday + weathersit + temp + atemp + hum + windspeed, data=df_train)
summary(lm_model_cnt)
predicted_cnt <- predict(lm_model_cnt, newdata = df_test)

# Plotting the Actual vs Predicted Total Users
ggplot(df_test, aes(x = cnt, y = predicted_cnt)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted for Total Users (cnt)") +
  xlab("Actual Total Users (cnt)") +
  ylab("Predicted Total Users (cnt)")

# Coefficients for the Total Users Regression Model
cnt_coef <- lm_model_cnt$coefficients
cnt_coef

# Assessing the Accuracy of the Total Users Regression Model
rsquared_cnt <- summary(lm_model_cnt)$r.squared
adj_rsquared_cnt <- summary(lm_model_cnt)$adj.r.squared
mse_cnt <- mean((df_test$cnt - predicted_cnt)^2)

## Linear Regression Model for Casual Users
# Building a Linear Regression Model for Casual Users
lm_model_casual <- lm(casual ~ season + mnth + holiday + weekday + workingday + weathersit + temp + atemp + hum + windspeed, data=df_train)
predicted_casual <- predict(lm_model_casual, newdata = df_test)

# Plotting the Actual vs Predicted Casual Users
ggplot(df_test, aes(x = casual, y = predicted_casual)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted for Casual Users") +
  xlab("Actual Casual Users") +
  ylab("Predicted Casual Users")

# Coefficients for Casual Users Regression Model
casual_coef <- lm_model_casual$coefficients
casual_coef

# Assessing the Accuracy of the Casual Users Regression Model
rsquared_casual <- summary(lm_model_casual)$r.squared
adj_rsquared_casual <- summary(lm_model_casual)$adj.r.squared
mse_casual <- mean((df_test$casual - predicted_casual)^2)


## Linear Regression Model for Registered Users
# Building a Linear Regression Model for Registered Users
lm_model_registered <- lm(registered ~ season + mnth + holiday + weekday + workingday + weathersit + temp + atemp + hum + windspeed, data=df_train)
predicted_registered <- predict(lm_model_registered, newdata = df_test)

# Plotting the Actual vs Predicted Registered Users
ggplot(df_test, aes(x = registered, y = predicted_registered)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted for Registered Users") +
  xlab("Actual Registered Users") +
  ylab("Predicted Registered Users")

# Coefficients for Registered Users Regression Model
registered_coef <- lm_model_registered$coefficients
registered_coef

# Assessing the Accuracy of the Registered Users Regression Model
rsquared_registered <- summary(lm_model_registered)$r.squared
adj_rsquared_registered <- summary(lm_model_registered)$adj.r.squared
mse_registered <- mean((df_test$registered - predicted_registered)^2)
