# ec349-a1

#Pre-Processing Multiple Data
library(jsonlite)

#Clear2
cat("\014")
rm(list=ls())

#Set working directory
setwd("/Users/shirleyzhu/Desktop/EC349 Assignment 1/Datasets")

#Load Data and .rda files
business_data <- stream_in(file("yelp_academic_dataset_business.json"))
checkin_data <- stream_in(file("yelp_academic_dataset_checkin.json"))
tip_data <- stream_in(file("yelp_academic_dataset_tip.json"))
load("yelp_review_small.rda") 
# review_id：a unique identifier of the review
# user_id：a unique identifier for the user who wrote the review
# business_id：the unique identifier of the business, representing the businesses that have been reviewed
# stars：users' ratings of a business (range from 1 to 5)
# useful：the number of times a review has been marked as "useful" by other users
# funny：the number of times a review has been marked as "funny" by other users
# cool：the number of times a review has been marked as "cool" by other users
# text：text content of the review, including the users' feedback and comments.
# date：the date and time indicating when the review was written

load("yelp_user_small.rda") 
# user_id：a unique identifier for the user who wrote the review
# name: name of the user
# review_count：number of reviews a user has written
# yelping_since：the date and time the user registered on Yelp
# useful：the number of times the user's comments were marked as "useful" by other users
# funny：the number of times the user's comments were marked as "funny" by other users
# cool：the number of times the user's comments were marked as "cool" by other users
# elite：whether a user is an 'Elite User' of Yelp, usually determined by their contribution and activity on the platform
# friends：the number of friends a user has, including the number of unique identifiers of the friends
# fans：number of fans of the user
# average_stars：average stars rating of the user
# compliment_hot：the number of times the user's reviews were marked as "hot" by other users
# compliment_more：the number of times the user's reviews were marked as "more" by other users
# compliment_profile：the number of times the user's reviews were marked as "profile" by other users
# compliment_cute：the number of times the user's reviews were marked as "cute" by other users
# compliment_list：the number of times the user's reviews were marked as "list" by other users
# compliment_note：the number of times the user's reviews were marked as "note" by other users
# compliment_plain：the number of times the user's reviews were marked as "plain" by other users
# compliment_cool：the number of times the user's reviews were marked as "cool" by other users
# compliment_funny：the number of times the user's reviews were marked as "funny" by other users
# compliment_writer：the number of times the user's reviews were marked as "writer" by other users
# compliment_photos：the number of times the user's reviews were marked as "photos" by other users

#Load libraries & install packages

install.packages("ggplot2")
library(ggplot2)
install.packages("lattice")
library(lattice)
install.packages("caret")
library(caret)
install.packages("glmnet")
library(glmnet)
install.packages("tm")
library(tm)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(stringr)
library(DT)      
install.packages("leaflet")
library(leaflet) 
library(SnowballC) 
install.packages("textcat")
library(textcat)
install.packages("corrplot")
library(corrplot)
library(knitr)
library(scales)
library(randomForest)
################################################################################
################################################################################

#Data Pre-processing
################################################################################

##'friends' - convert to numeric and count the number of friends (generate a new column named 'num_friends')
user_data_small <- user_data_small %>%
  mutate(
    friends_count = ifelse(friends == "None", 0, lengths(strsplit(friends, ", "))),
    num_friends = as.numeric(friends_count)
  ) %>%
  select(-friends_count)

str(user_data_small$num_friends)

##'yielping_since'
user_data_small <- user_data_small %>%
  mutate(yelping_since_date = as.POSIXct(yelping_since, format = "%Y-%m-%d", tz = "UTC"))

str(user_data_small$yelping_since_date)

##'date' - change to date format and create a new column named 'review_date'
review_data_small <- review_data_small %>%
  mutate(review_date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

str(review_data_small$review_date)

################################################################################

##Merge user_data_small and review_data_small using the common user ids (user_id)
merged_data <- review_data_small %>%
  inner_join(user_data_small, by = "user_id")

dim(merged_data) #279878 observations and 30 variables

#Select columns that might be relevant
merged_data <- merged_data %>%
  select(stars, review_count, useful.x, funny.x, cool.x, 
         elite, fans, average_stars, compliment_hot, compliment_more, 
         compliment_profile, compliment_cute, compliment_list, compliment_note, 
         compliment_plain, compliment_cool, compliment_funny, compliment_writer, 
         compliment_photos)

summary(merged_data)
str(merged_data)
################################################################################
#Distribution of review count and plot a pie chart to illustrate
#Count the number who gave reviews and the number of users who didn't
users_without_reviews <- merged_data %>%
  filter(review_count == 0)
users_with_reviews <- merged_data %>%
  filter(review_count > 0)

#Create a summary table for review_count
summary_table <- data.frame(
  Category = c("Users with Reviews", "Users without Reviews"),
  Count = c(nrow(users_with_reviews), nrow(users_without_reviews))  )

kable(summary_table, format = "markdown")

ggplot(merged_data, aes(x = review_count)) +
  geom_histogram(binwidth = 1, fill = "tomato", color = "black") +
  labs(title = "Figure 1. Distribution Review Count (in logs)",
       x = "Number of Reviews (in log)",
       y = "Number of Users")  +
  scale_x_log10() +
  theme_minimal() +
  scale_y_continuous(labels = comma_format())

##Check for missing values
sum(is.na(merged_data)) #no missing values in the merged data.

#'elite'
merged_data$elite <- ifelse(merged_data$elite != "", 1, 0)
merged_data$elite <- as.numeric(merged_data$elite)

str(merged_data$elite)
################################################################################

##Distribution of stars in merged_data
table(merged_data$stars)
ggplot(merged_data, aes(x = stars, fill = stars)) +
  geom_bar(color = "black") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 3 ) +
  labs(title = "Figure 2. Distribution of Stars Rating", x = "Stars", y = "Number of Reviews") +
  theme_minimal() +
  scale_y_continuous(labels = comma_format())

summary(merged_data$stars)

#Calculate the correlation matrix
cor_matrix <- cor(merged_data[, sapply(merged_data, is.numeric)])
print(cor_matrix)

#Visual representation of the correlation matrix
corrplot(cor_matrix, method = "color", tl.col = "black", tl.cex = 0.7)
title("Figure 3. Correlation Matrix")
###############################################################################

#First convert the dependent variable (stars) to factor type
merged_data$stars <- as.factor(merged_data$stars)

set.seed(123)  
sampled_data <- merged_data[sample(nrow(merged_data), 2000), ] #increase speed, sample a subset of 2000 observations

#Employment of the recursive feature elimination to select the relevant features
control <- rfeControl(functions=rfFuncs, method="cv", number=5) #set control parameters for RFE, use random forest functions and a 5-fold Cross-Validation throughout the process
results <- rfe(sampled_data[, -which(names(sampled_data) == "stars")], #select predictor variables, exclude the 'stars' column, since this consists of the dependent variable
               sampled_data$stars, sizes=c(1:5), rfeControl=control) #evaluate subsets of features from 1 to 5

#View results for the best feature set
print(results)

#Select optimal features
opt_features <- results$optVariables
opt_features
################################################################################

#Split into testing and training data
set.seed(1)
data <- merged_data[sample(nrow(merged_data), 200000), ] #sample a subset of 200,000 observations increase the speed in training the model.
splitIndex <- createDataPartition(data$stars, p = 0.8, list = FALSE) #since the merged data has 279878 observations, we can increase the number of observations while generating the testing data to enhance performance.
training_data <- data[splitIndex, ]
testing_data <- data[-splitIndex, ]

dim(testing_data) #39997 observations, 19 columns
dim(training_data) #160003 observations, 19 columns

# Train the Random Forest model
rf_model <- randomForest(stars ~ ., data = training_data[, c(opt_features, "stars")], ntree = 100, mtry = 3) #set 100 trees, three features at each split. 

#Check for model highlights
print(rf_model)

# Predict the number of stars
predictions <- predict(rf_model, testing_data[, opt_features])

# Evaluate the predictions derived from model
confusionMatrix(predictions, testing_data$stars)
