library(readr)
library(dplyr)

getSpotify <-
  function(region = "br",
           date_init = "2018-01-10",
           date_final = "2019-10-04",
           period = "daily",
           period2 = "days") {
    
    date <- seq.Date(as.Date(date_init), as.Date(date_final), by = period2)
    date <- date[-length(date)]
    
    url1 <- 'https://spotifycharts.com/regional/'
    url <- paste0(url1, region, "/", period, "/", date_final, "/download")
    
    df <- read_csv(url, skip = 1)
    df <- df %>% mutate(Date = as.Date(date_final))
    
    for(i in c(1:length(date))){
      url <- paste0(url1, region, "/", period, "/", date[i], "/download")
      df_ <- read_csv(url, skip = 1)
      df_ <- df_ %>% mutate(Date = date[i])
      df <- rbind(df, df_)
    }

    return(df)
  }

df <- read_csv("spotify-br-2018-2020.csv")
df3 <- getSpotify(date_init = '2020-10-06', date_final = '2020-10-13')

# Check
df3 %>% group_by(Date) %>% count()

df <- rbind(df, df3)
write.csv(df,"spotify-br-2018-2020.csv", row.names = F)
