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

init_neg <- (table(review_data$Stars) / sum(table(review_data$Stars)))[1]
init_pos <- (table(review_data$Stars) / sum(table(review_data$Stars)))[2]

review_data$Comment <- (tokenize_words(review_data$Comment,
                               lowercase = TRUE,
                               strip_punct = TRUE,
                               strip_numeric = TRUE,
                               stopwords = stopwords::stopwords("en")))

calc_Probs <- function(tokens) {
  counts <- table(unlist(tokens)) + 1
  (counts/sum(counts))
  
}

pos_review_data <- subset(review_data, Stars == "positive")
neg_review_data <- subset(review_data, Stars == "negative")

pos_probs <- calc_Probs(pos_review_data$Comment)
neg_probs <- calc_Probs(neg_review_data$Comment)