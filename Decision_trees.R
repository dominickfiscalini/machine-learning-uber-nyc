library(dplyr)
library(tidyverse)
library(party)

df_uber <- read.csv("https://query.data.world/s/kbc7dwu7ee5l2useaijcwstj7swg7z", header=TRUE, stringsAsFactors=FALSE);
df_uber <- df_uber[grepl('^.*[A-z].*', df_uber$PuFrom),] #get rid of rows with empty city entry
df_uber <- df_uber[grepl('^.*[A-z].*', df_uber$Street),] #get rid of rows with empty street entry
df_uber <- df_uber[!grepl(' ', df_uber$Address),]
df_uber <- df_uber[order(df_uber$PuFrom),] #put in alphabetical order with respect to city name
df_uber <- df_uber[grepl("NY", df_uber$State),] #get rid of rows with states other than new 
df_uber$PuFrom <- toupper(df_uber$PuFrom) #turn city names to all caps
df_uber$Street <- toupper(df_uber$Street) #turn street names to all caps
table(df_uber$PuFrom)

pickup_city <- function(city) { #put above line into a function, but function argument does not have to be all caps
  city <- toupper(city)
  df_uber_city = df_uber[grepl(city, df_uber$PuFrom),]
  return(df_uber_city)
}

df_uber_bronx <- pickup_city("Bronx")
df_uber_brooklyn <- pickup_city("Brooklyn")
df_uber_combine <- rbind(df_uber_bronx, df_uber_brooklyn)
df_uber_combine$PuFrom <- gsub(" ", "", df_uber_combine$PuFrom, fixed = TRUE)
df_uber_combine <- df_uber_combine[!grepl('BRONXVILLE', df_uber_combine$PuFrom),]
table(df_uber_combine$PuFrom)
str(df_uber_combine)


df_uber_combine <- select(df_uber_combine, -c("State", "Address", "Street"))
df_uber_combine$PuFrom <- as.factor(df_uber_combine$PuFrom)
df_uber_combine$Date <- gsub(".", "", df_uber_combine$Date, fixed = TRUE)
df_uber_combine$Date <- strtoi(df_uber_combine$Date)
df_uber_combine$Time <- gsub(":", "", df_uber_combine$Time, fixed = TRUE)
df_uber_combine$Time <- strtoi(df_uber_combine$Time)

set.seed(123)
df_upartition <- sample(2, nrow(df_uber_combine), replace = TRUE, prob = c(0.8, 0.2))

df_utrain <- df_uber_combine[df_upartition == 1, ]
df_utest <- df_uber_combine[df_upartition == 2, ]
formula = PuFrom ~ Date + Time

uber_tree1 <- ctree(formula, data = df_utrain, 
                    controls = ctree_control(minsplit = 1))
plot(uber_tree1)

