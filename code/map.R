library(tidyverse)  # pipe operator manipulation
library(magrittr)   # pipe operator mutation
library(lubridate)
library(tm)

setwd("C:\\Users\\User\\Desktop\\GitHub Projects\\TeenPregnancyTweets")

load("C:\\Users\\User\\Desktop\\data_list.RData")

# convert lists to dataframe
tweets.raw.df <- data_list %>% 
  map_df(.f = ~ data.frame(
    # select non-user related data.
    ID = .x$tweet_id,
    Created_At = .x$timestamp,
    Text = .x$text,
    #account = .x$screen_name,
    stringsAsFactors = FALSE
  )
  ) %>% 
  as_tibble()

# reformat date column using magrittr and lubridate
tweets.raw.df %<>% 
  mutate(
    Created_At = Created_At %>% 
      # remove undesirable string
      str_remove_all(pattern = '\\T') %>%
      # parse date
      parse_date_time(orders = '"Ymd HMS"',locale = "English" )
  )

# subtract seconds (otherwise time series too large)
tweets.raw.df %<>% 
  mutate(Created_At = Created_At - 5*60*60)

tweets.raw.df %<>% 
  mutate(Created_At_Round = Created_At %>% round(units = 'mins') %>% as.POSIXct())


# efficient custom stop words removal function
RemoveStopwordsFromText <- function(texto, # text
                                    swords # stopwords list
){
  sapply(texto, function(x){
    y <- strsplit(x," ",T)[[1]]
    paste(y[!(y%in%swords)],collapse=" ")
  }, USE.NAMES = FALSE
  )
}

# basic stopwords list
sw <- readLines("https://github.com/FoxHound112263/Colombia-Pact-Text/raw/master/data/entrada/stop_words_spanish.txt",warn = F)


# second custom cleaning function (not used for now)
tweets.df <- tweets.raw.df %>% 
  # Remove column.
  select(-  Created_At) %>% 
  # Convert to lowercase. 
  mutate(Text = Text %>% str_to_lower) %>% 
  # Remove unwanted characters. 
  mutate(Text= Text %>% str_remove_all(pattern = '\\n')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = '&amp')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = 'https')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = 'http')) %>% 
  # Remove hashtags.
  mutate(Text = Text %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts.
  mutate(Text = Text %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>% 
  # Remove retweets.
  mutate(Text = Text %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
  mutate(Text = Text %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = '\\_'))


# remove accents 
replacement.list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')

tweets.df %<>% 
  mutate(Text = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                       new = replacement.list %>% str_c(collapse = ''),
                       x = Text))


# remove stopwords
tweets.df$Text <- sapply(tweets.df$Text, function(x) RemoveStopwordsFromText(x,sw))

# convert the text to a corpus object using tm
corpus <-  Corpus(x = VectorSource(x = tweets.df$Text))

# extra cleaning using tm_map
tweets.text <- corpus %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('spanish')) %>% 
  tm_map(PlainTextDocument) #%>% 
# stemming (in spanish this mostly distorts the words) 
#  tm_map(stemDocument, 'spanish')

# bring data to tibble
tweets.df %<>% mutate(Text = tweets.text[[1]]$content)

# extra cleaning
# efficient custom clean function specifically for twitter
preproctext <- function(x){
  require(magrittr)
  x[which(is.na(x))] <- ""
  y <- x %>% 
    iconv(.,from="utf-8",to="ASCII//TRANSLIT") %>%
    gsub("[^[:print:]]", " ", .) %>%
    gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    gsub("[[:space:]]{1,}", " ", .) %>%
    #tolower %>% 
    # remove large strings
    gsub("\\b[[:alpha:]]{14,}\\b", " ", .) %>%
    trimws
  return(y)
}

tweets.df$Text <- preproctext(tweets.df$Text)

#---------------------------------------------------------------------------------------

# load municipios data

municipios <- read.delim("http://blog.jorgeivanmeza.com/wp-content/uploads/2008/09/municipioscolombiacsv.txt",
                         sep = ",",fileEncoding = "UTF-8",
                         col.names = c("idm","nombre_m","idp","nombre_d","latitude","longitude"),
                         stringsAsFactors = F)


municipios$latitude <- lapply(municipios$latitude, as.numeric)

# convert the text to a corpus object using tm
corpus <-  Corpus(x = VectorSource(x = municipios$nombre_m))

# extra cleaning using tm_map
municipios.text <- corpus %>% 
  tm_map(removePunctuation) %>%
  tm_map(str_to_lower) %>% 
  tm_map(removeNumbers) %>% 
  #tm_map(removeWords, stopwords('spanish')) %>% 
  tm_map(PlainTextDocument)

municipios %<>% mutate(municipios_m = municipios.text[[1]]$content)

# remove accent
municipios %<>% 
  mutate(municipios_m = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                       new = replacement.list %>% str_c(collapse = ''),
                       x = municipios_m))

municipios <- municipios[complete.cases(municipios), ]

municipios$municipios_m <- gsub('bogota dc', 'bogota', municipios$municipios_m)

library(stringi)


freq <- c()
for (i in municipios$municipios_m) {
  temp <- length(grep(as.character(i), tweets.df$Text))
  freq[i] <- temp
}


freq <- data.frame(freq)
freq <- data.frame(municipios_m = rownames(freq), freq = freq$freq)


df <- full_join(municipios, freq, by = "municipios_m")


# remove municipios with high frequency
remove_m <- c("achi","anza", "andes", "apia", "colombia", "concepcion",
              "dolores", "gonzalez", "iles", "iza", "lloro", "mani", "morales", 
              "pacho", "peque", "remedios", "silvia", "tado", "tame", "tena", "toca", "tota", 
              "tona","une")

# exclude
df <- df[ !grepl(paste(remove_m, collapse="|"), df$municipios_m),]

df$frequency <- NULL

#---------------------------------------------------------------------------------------

df <- filter(df, df$freq > 0)



# MAP
library(leaflet)
library(leaflet.minicharts)


map <-  leaflet(df) %>% addTiles() %>%
  addCircleMarkers(~longitude, ~as.numeric(latitude),
    radius = ~ sqrt(freq) + 1,
    stroke = FALSE, fillOpacity = 0.5,
    popup = paste(df$municipios_m,":",df$freq,sep = " ")
  ) #%>% 
  #addProviderTiles("Stamen.TonerLite")

map

m <- leaflet(data = df) %>%
  addTiles() %>%
  #addCircleMarkers() %>% 
  addCircleMarkers(~longitude, ~as.numeric(latitude),
                   radius = ~ sqrt(freq) + 1,
                   
    popup = paste0(
    "<div>",
    "<h3>",
    df$municipios_m,
    "</h3>",
    "<strong>Número de menciones</strong>: ",
    as.character(df$freq),
    "</br>",
    "<strong>Departamento</strong>: ",
    df$nombre_d,
    "</br>",
    #"<strong>Meta</strong>: ",
    #data$Meta.1,
    "</div>"
  ),
  label= df$municipios_m,
  #icon = icon.ion,
  stroke = F, fillOpacity = 0.7,
  )

m


save(m, file = "datamarkdown\\map.RData")

