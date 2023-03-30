library(stopwords)
library(tokenizers)
library(SnowballC)
library(dplyr)

setwd("C:/Users/Dominick/Documents/GitHub/machine-learning-uber-nyc")
review_data <- read.csv("Uber-Review-Data.csv")
review_data <- subset(review_data, select = -c(Date))
review_data <- subset(review_data, Stars != 3)
for(i in 1:nrow(review_data)){
  if(review_data$Stars[i] < 3){
    review_data$Stars[i] = "negative"
  }
  else if(review_data$Stars[i] == 4){
    review_data$Stars[i] = "positive"
  }
  else if(review_data$Stars[i] == 5){
    review_data$Stars[i] = "positive"
  }
}
  
table(review_data$Stars)

review_partition <- sample(2, nrow(review_data), replace = TRUE, prob = c(0.8, 0.2))
review_train <- review_data[review_partition == 1, ]
review_test <- review_data[review_partition == 2, ]

init_neg <- (table(review_train$Stars) / sum(table(review_train$Stars)))[1]
init_pos <- (table(review_train$Stars) / sum(table(review_train$Stars)))[2]

review_train$Comment <- (tokenize_words(review_train$Comment,
                               lowercase = TRUE,
                               strip_punct = TRUE,
                               strip_numeric = TRUE,
                               stopwords = stopwords::stopwords("en")))

calc_Probs <- function(tokens) {
  counts <- table(unlist(tokens)) + 1
  (counts/sum(counts))
  
}

pos_review_data <- subset(review_train, Stars == "positive")
neg_review_data <- subset(review_train, Stars == "negative")

pos_probs <- calc_Probs(pos_review_data$Comment)
neg_probs <- calc_Probs(neg_review_data$Comment)

classify_comment <- function(input) {
  
  test <- unlist(tokenize_words(input,
                                lowercase = TRUE,
                                strip_punct = TRUE,
                                strip_numeric = TRUE,
                                stopwords = stopwords::stopwords("en")))
  
  pos_pred <- init_pos*ifelse(is.na(prod(pos_probs[test])), 1, 
                              prod(pos_probs[test]))
  neg_pred <- init_neg*ifelse(is.na(prod(neg_probs[test])), 1, 
                              prod(neg_probs[test]))
  
  if(pos_pred > neg_pred){
    return("Positive")
  }   else{"Negative"}                                                    
} 

y_pred = c(length = nrow(review_test))
for(i in 1:nrow(review_test)){
  y_pred[i] = classify_comment(review_test$Comment[i])
}
y_real = review_test$Stars
y_real
y_pred
table(Predicted = y_pred, Actual = y_real)

accuracy = (302+14)/(302+36+90+14)
accuracy
