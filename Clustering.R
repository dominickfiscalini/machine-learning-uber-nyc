install.packages("factoextra")
install.packages("lsa")

library(factoextra)
library(dplyr)
library(tidyverse)

install.packages("rvest") #allows us to parse HTML content and extract the HTML elements from it. 
install.packages("xml2")

library(tidyverse) # data cleaning. manipulation and visualization 
library(xml2)
library(rvest)
library(dplyr)
library(lsa)

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


zillowclust_labels <- df_zillow$bathrooms
for(i in 1:length(zillowclust_labels)){
  zillowclust_labels[i] <- paste(zillowclust_labels[i], "Bathroom(s)", sep = " ")
}
zillowclust_data <- df_zillow[, c(8,10,11)]
zillowclust_data$zpid <- strtoi(zillowclust_data$zpid)

zillow_data_scale <- scale(zillowclust_data)
zillow_dist <- dist(zillow_data_scale)


fviz_nbclust(zillow_data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

zillow_km <- kmeans(zillow_data_scale, centers = 6, nstart = 100)
zillow_clusters <- zillow_km$cluster
zillow_clusters

fviz_cluster(list(data=zillow_data_scale, cluster = zillow_km$cluster))

zillow_table <- table(zillow_clusters, zillowclust_labels)

zillow_hc_data <- t(zillowclust_data)
colnames(zillow_hc_data) <-  paste(zillowclust_labels , 1:ncol(zillow_hc_data), sep="")
zillow_hc_scale <- scale(zillow_hc_data)

zillow_cosine <- cosine(zillow_hc_scale)
zillow_dist_hc <- dist(zillow_cosine)
zillow_dist_hc

zillow_hc <- hclust(zillow_dist, method = "complete")
zillow_hc

plot(zillow_hc)
rect.hclust(zillow_hc, k=5, border = 2:5)


