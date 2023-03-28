library(tidyverse) # data cleaning. manipulation and visualization 
library(xml2)
library(rvest)
library(dplyr)
library(party)

library("httr")
url <- "https://zillow-com1.p.rapidapi.com/propertyExtendedSearch"

zillow_info <- function(city, hometype) {
  city <- tolower(city)
  city_state <- paste(city, ", ny", sep = "")
  
  queryString <- list(
    location = city_state,
    home_type = hometype
  )
  
  api_call <- VERB("GET", url, 
                   add_headers('X-RapidAPI-Key' = '75f20279e3msh97c0acde2af9914p1a6fd4jsn36a96f543f1e',
                               'X-RapidAPI-Host' = 'zillow-com1.p.rapidapi.com'), 
                   query = queryString, content_type("application/octet-stream"))
  api_call$status_code
  
  api_char <- base::rawToChar(api_call$content)
  
  api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)
  
  df_zillow <- api_JSON$props
  
  for(i in 1:nrow(df_zillow)) {
    df_zillow$dateSold[i] <- "Not Sold"
  }
  df_zillow$dateSold
  
  df_zillow = df_zillow %>% drop_na(zestimate)
  df_zillow[["priceChange"]][is.na(df_zillow[["priceChange"]])] <- 0
  df_zillow = df_zillow[, colSums(is.na(df_zillow))==0]
  df_zillow = select(df_zillow, -c("daysOnZillow", "currency", "hasImage"))
}

df_zillow <- zillow_info("new york city", "Houses")

df_zillow_tree <- df_zillow
df_zillow_tree$err = df_zillow_tree$zestimate - df_zillow_tree$price
df_zillow_tree$thres = 0.02 * df_zillow_tree$price
df_zillow_tree$accuracy = 0
for(i in 1:nrow(df_zillow_tree)){
  if(abs(df_zillow_tree$err[i]) < df_zillow_tree$thres[i]){
    df_zillow_tree$accuracy[i] = 1
  }
}
set.seed(1234)
df_zpartition <- sample(2, nrow(df_zillow_tree), replace = TRUE, prob = c(0.7, 0.3))

df_ztraining <- df_zillow_tree[df_zpartition == 1, ]
df_ztest <- df_zillow_tree[df_zpartition == 2, ]

tree1 <- ctree(accuracy ~ latitude + longitude + price, data = df_ztraining)
plot(tree1)
set.seed(1234)
tree2 <- ctree(accuracy ~ latitude + longitude, data = df_ztraining)
plot(tree2)
set.seed(1234)
tree3 <- ctree(accuracy ~ bathrooms + lotArea, data = df_ztraining)
plot(tree3)
tree2