install.packages("rvest") #allows us to parse HTML content and extract the HTML elements from it. 
install.packages("xml2")
install.packages("colorspace")
install.packages("ggplot2", dependencies = TRUE)
install.packages("scales")

library(tidyverse) # data cleaning. manipulation and visualization 
library(xml2)
library(rvest)
library(dplyr)
library(ggplot2)
library(colorspace)

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

df_uber <- read.csv("https://query.data.world/s/kbc7dwu7ee5l2useaijcwstj7swg7z", header=TRUE, stringsAsFactors=FALSE);

df_uber <- df_uber[grepl('^.*[A-z].*', df_uber$PuFrom),] #get rid of rows with empty city entry
df_uber <- df_uber[grepl('^.*[A-z].*', df_uber$Street),] #get rid of rows with empty street entry
df_uber <- df_uber[order(df_uber$PuFrom),] #put in alphabetical order with respect to city name
df_uber <- df_uber[grepl("NY", df_uber$State),] #get rid of rows with states other than new york
df_uber$PuFrom <- toupper(df_uber$PuFrom) #turn city names to all caps
df_uber$Street <- toupper(df_uber$Street) #turn street names to all caps

pickup_city <- function(city) { #put above line into a function, but function argument does not have to be all caps
  city <- toupper(city)
  df_uber_city = df_uber[grepl(city, df_uber$PuFrom),]
  return(df_uber_city)
}

plot1 <- ggplot(data = df_uber, mapping = aes(x = Time, y= PuFrom)) + 
  geom_point(size  = 2 ) + geom_line(color = "red")
plot1
plot2 <- ggplot(data = df_zillow, mapping = aes(x = bathrooms)) + 
  geom_histogram(binwidth = 1)
plot2
plot3 <- ggplot(df_zillow, aes(factor(bathrooms), price)) + 
  geom_boxplot()
plot3
plot4 <- ggplot(data = df_zillow, mapping = aes(x = bedrooms)) + 
  geom_histogram(binwidth = 1)
plot4
plot5 <- ggplot(df_zillow, aes(factor(bedrooms), price)) + 
  geom_boxplot()
plot5
plot6 <- ggplot(data = df_zillow, mapping = aes(latitude,price)) + 
  geom_point(size  = 2 )
plot6
plot7 <- ggplot(data = df_zillow, mapping = aes(longitude,price)) + 
  geom_point(size  = 2 )
plot7
plot8 <- ggplot(data = df_zillow, mapping = aes(zpid,price)) + 
  geom_point(size  = 2 )
plot8
plot9 <- ggplot(data = df_zillow, mapping = aes(zestimate,price)) + 
  geom_point(size  = 2 )
plot9
plot10 <- ggplot(data = df_uber, mapping = aes(x = PuFrom)) + 
  stat_count(width = 1)
plot10
