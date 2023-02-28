df_uber <- read.csv("https://query.data.world/s/kbc7dwu7ee5l2useaijcwstj7swg7z", header=TRUE, stringsAsFactors=FALSE);

df_uber <- df_uber[grepl('^.*[A-z].*', df_uber$PuFrom),] #get rid of rows with empty city entry
df_uber <- df_uber[grepl('^.*[A-z].*', df_uber$Street),] #get rid of rows with empty street entry
df_uber <- df_uber[order(df_uber$PuFrom),] #put in alphabetical order with respect to city name
df_uber <- df_uber[grepl("NY", df_uber$State),] #get rid of rows with states other than new york
df_uber$PuFrom <- toupper(df_uber$PuFrom) #turn city names to all caps
df_uber$Street <- toupper(df_uber$Street) #turn street names to all caps

df_uber_airmont <- df_uber[grepl("AIRMONT", df_uber$PuFrom),] #pickups only in airmont (example)
pickup_city <- function(city) { #put above line into a function, but function argument does not have to be all caps
  city <- toupper(city)
  df_uber_city = df_uber[grepl(city, df_uber$PuFrom),]
  return(df_uber_city)
}


