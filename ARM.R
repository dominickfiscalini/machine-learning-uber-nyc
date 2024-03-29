install.packages("rvest") #allows us to parse HTML content and extract the HTML elements from it. 
install.packages("xml2")
install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)
library(tidyverse) # data cleaning. manipulation and visualization 
library(xml2)
library(rvest)
library(dplyr)

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

df_zillow_arm <- df_zillow[, c(2,6,8,10,11,12)]
price_mean <- mean(df_zillow$price)
zest_mean <- mean(df_zillow$zestimate)
df_zillow_arm$zpid <- strtoi(df_zillow$zpid)
zpid_mean <- mean(df_zillow_arm$zpid)
rent_mean <- mean(df_zillow$rentZestimate)
bath_mean <- mean(df_zillow$bathrooms)

lst <- vector(mode='list', length=nrow(df_zillow))

for(i in 1:nrow(df_zillow)){
  if(df_zillow_arm$zpid[i] >= zpid_mean){
    # df_zillow_arm$zpid[i] <- ""
    lst[i] <- i
  }else{
    lst[i] <- 0
    # df_zillow_arm$zpid[i] <- "High ZPID"
  }
}

for(i in 1:length(lst)){
  if(lst[i] == 0){
    df_zillow_arm$zpid[i] <- ""
  }
  else{
    df_zillow_arm$zpid[i] <- "High ZPID"
  }
}

for(i in 1:nrow(df_zillow)){
  if(df_zillow_arm$price[i] >= price_mean){
    df_zillow_arm$price[i] <- "High Price"
  }
  else{
    df_zillow_arm$price[i] <- ""
  }
}

for(i in 1:nrow(df_zillow)){
  if(df_zillow_arm$zestimate[i] >= zest_mean){
    df_zillow_arm$zestimate[i] <- "High Zestimate"
  }
  else{
    df_zillow_arm$zestimate[i] <- ""
  }
}

for(i in 1:nrow(df_zillow)){
  if(df_zillow_arm$rentZestimate[i] >= rent_mean){
    df_zillow_arm$rentZestimate[i] <- "High Rent Zestimate"
  }
  else{
    df_zillow_arm$rentZestimate[i] <- ""
  }
}

for(i in 1:nrow(df_zillow)){
  if(df_zillow_arm$bathrooms[i] >= bath_mean){
    df_zillow_arm$bathrooms[i] <- "Bathrooms"
  }
  else{
    df_zillow_arm$bathrooms[i] <- ""
  }
}

rule1 <- apriori(df_zillow_arm, parameter=list(support = 0.5, confidence = 0.5))
inspect(head(sort(rule1, by = "confidence"),15))

plot1 <- plot(rule1)
plot1

plot2 <- plot(rule1, method = "grouped")
plot2
