library(reticulate) # python scripts
library(tidyverse)  # pipe operator manipulation
library(magrittr)   # pipe operator mutation
library(lubridate)  # date manipulation
library(plotly)     # interactive plots (ggplot2)
library(tm)         # text mining
library(tidytext)   # tidy models
library(wordcloud2) # word clouds
library(tsne)       # non-linear dim reduction
library(slam)       # deal with BIG tdm matrices freq
library(igraph)     # network analysis
library(networkD3)  # network visualization
library(glue)       # format and interpolate strings
library(widyr)      # correlation analysis
#library(syuzhet)    # sentiment analysis
#library(graphTweets) # accounts retweet networks
#library(quanteda)   # cosine matrix

#-------------------------------------------------------------
# PYTHON ENVIRONMENT FOR MINICONDA (download on prompt)

#conda_create(envname = NULL, conda = "auto")
#conda_remove(NULL, packages = NULL, conda = "auto")


#py_install("pymongo", pip = TRUE)
#py_install("pandas", pip = TRUE)

# set working directory
setwd("C:\\Users\\User\\Desktop\\GitHub Projects\\TeenPregnancyTweets")

# connect to mongo database using reticulate
py_run_string("

import pymongo
import pandas as pd
client = pymongo.MongoClient('mongodb://54.90.49.105:27017', username='rw_admin_smdt', password='HolocaustoDel84', authSource='smdt')

db = client['smdt']
tweets_collection = db['tweets']
tweet_list = [tweet for tweet in tweets_collection.find({})]
              
              ")

# store python in R
data <-  py_run_string("tweet_dt = pd.DataFrame(tweet_list)")
# convert and extract R object
data_r <- py_to_r(data)
data_list <- data_r$tweet_list
#save(data_list, file = "data_list.RData")


# remove '_id' since it's a python strange object (optional)
i = 0
for (i in 1:range(length(data_list))) {
  data_list[[i]][1] <- NULL
  i = i + 1
}
rm(i)

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

# glimpse of tweets text removing users
tweets.raw.df %>% 
  filter(!str_detect(string = Text, pattern = '@')) %>% 
  head()

# save object for markdown
data.glimpse <- tweets.raw.df %>% 
  filter(!str_detect(string = Text, pattern = '@')) %>% 
  head()

#save(data.glimpse, file = "datamarkdown\\data.glimpse.RData")


# reformat date column using magrittr and lubridate
tweets.raw.df %<>% 
  mutate(
    Created_At = Created_At %>% 
      # remove undesirable string
      str_remove_all(pattern = '\\T') %>%
      # parse date
      parse_date_time(orders = '"Ymd HMS"',locale = "English" )
  )

# glimpse of tibble in correct format
tweets.raw.df %>% 
  filter(!str_detect(string = Text, pattern = '@')) %>% 
  head()


#-------------------------------------------------------------
# TIME ANALYSIS

# subtract seconds (otherwise time series too large)
tweets.raw.df %<>% 
  mutate(Created_At = Created_At - 5*60*60)

# time range
tweets.raw.df %>% pull(Created_At) %>% min()

tweets.raw.df %>% pull(Created_At) %>% max() #13 years!

# ignore seconds and keep minutes rounding them in new column
tweets.raw.df %<>% 
  mutate(Created_At_Round = Created_At %>% round(units = 'mins') %>% as.POSIXct())

# Plot the time series with plotly
plt <- tweets.raw.df %>% 
  count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Fecha') +
  ylab(label = NULL) +
  ggtitle(label = 'Número de Tweets por minuto (11 de septiembre de 2007 - 31 de mayo de 2020)')

plt %>% ggplotly()

#save plot
save(plt, file = "datamarkdown\\plt.RData")


# there is an interesting peak at around 2015-03-20 21:53:00
# let's see

results.time <- as.POSIXct(x = '2015-03-20 21:53:00')

tweets.raw.df %>% 
  filter(Created_At_Round > results.time ) %>% 
  select(Text) %>% 
  filter(!str_detect(string = Text, pattern = '@')) %>% 
  pull(Text) %>% 
  tail() 

# save object
pico <- tweets.raw.df %>% 
  filter(Created_At_Round > results.time ) %>% 
  select(Text) %>% 
  filter(!str_detect(string = Text, pattern = '@')) %>% 
  pull(Text) %>% 
  tail() 

save(pico, file = "datamarkdown\\pico.RData")

#-------------------------------------------------------------
# TEXT CLEANING


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



# extract hashtags function
GetHashtags <- function(tweet) {
  
  hashtag.vector <- str_extract_all(string = tweet, pattern = '#\\S+', simplify = TRUE) %>% 
    as.character()
  
  hashtag.string <- NA
  
  if (length(hashtag.vector) > 0) {
    
    hashtag.string <- hashtag.vector %>% str_c(collapse = ', ')
    
  } 
  
  return(hashtag.string)
}

# hashtags only
hashtags.df <- tibble(
  Hashtags = tweets.raw.df$Text %>% map_chr(.f = ~ GetHashtags(tweet = .x))
)

# Lots of tweets without hashtags
hashtags.df %>% head(20)

# Merge the dataframes
tweets.df %<>% bind_cols(hashtags.df) 


#-------------------------------------------------------------
# COUNTS

# extra stopwords added through an iterative process
extra.stop.words <- c('com','www','ly','twitter','pic','bit','fb','d')

# stopwords dataframe
stopwords.df <- tibble(
  word = c(stopwords(kind = 'es'), 
           # in case there are english tweets
           stopwords(kind = 'en'),  
           extra.stop.words)
)

# remove additional stopwords
words.df <- tweets.df %>% 
  unnest_tokens(input = Text, output = word) %>% 
  anti_join(y = stopwords.df, by = 'word')

# word count
word.count <- words.df %>% count(word, sort = TRUE)

# glimpse
word.count %>% head(20)

# bar plot
plt2 <- word.count %>% 
  # Set count threshold. 
  filter(n > 700) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'black', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Palabras más frecuentes')

plt2 %>% ggplotly()

# save plot
save(plt2, file = "datamarkdown\\plt2.RData")



# wordcloud
wc <- wordcloud2(word.count, color = 'random-dark',size = 1.1)
wc
#save wc
save(wc, file = "datamarkdown\\wc.RData")

# TFIDF weighting

# corpus with clean text
corpus2 <-  Corpus(x = VectorSource(x = tweets.df$Text))

# term document matrix with tf-idf
tdm = TermDocumentMatrix(corpus2,
                         control = list(weighting = weightTfIdf,
                                        stopwords = sw, 
                                        removePunctuation = T,
                                        removeNumbers = T,
                                        stemming = F))
# big sparse matrix
tdm

# using slam 'row_sums' for big matrix
freq <- as.data.frame(x = sort(row_sums(tdm), decreasing = F))
colnames(freq) <- 'freq'

# lot of noise
plot(sort(freq$freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

# convert to matrix for easy sorting
freq <- as.matrix(freq)

# sorting
high.freq <- freq[order(-freq),] 
high.freq <-  high.freq[1:20]

# dataframe 
hfp.df <- as.data.frame(high.freq[1:20])
hfp.df$names <- rownames(hfp.df)
colnames(hfp.df) <- c('n','word')

plt3 <- as.data.frame(hfp.df) %>% 
  # Set count threshold. 
  ggplot(aes(reorder(word,n), y = n)) +
  theme_light() + 
  geom_col(fill = 'black', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Palabras más frecuentes con ponderación (TF-IDF)')

plt3 %>% ggplotly()

save(plt3, file = "datamarkdown\\plt3.RData")



# fixing scale for wordcloud
hfp.df <-  hfp.df %>% 
  mutate(n = n / 10)

hfp.df <- hfp.df[c("word", "n")]

# TF-IDF wordcloud
wc2 <-  wordcloud2(hfp.df)

save(wc2, file = "datamarkdown\\wc2.RData")

#-------------------------------------------------------------
# HASHTAGS

# time of hastags
hashtags.unnested.df <- tweets.df %>% 
  select(Created_At_Round, Hashtags) %>% 
  unnest_tokens(input = Hashtags, output = hashtag)

# unique hashtag count
hashtags.unnested.count <- hashtags.unnested.df %>% 
  count(hashtag) %>% 
  drop_na()

# hashtag wordcloud
wordcloud2(hashtags.unnested.count)

# custom hashtag evolution
plt <- hashtags.unnested.df %>% 
  filter(hashtag %in% c('embarazo', 'aborto')) %>% 
  count(Created_At_Round, hashtag) %>% 
  ggplot(mapping = aes(x  = Created_At_Round, y = n, color = hashtag)) +
  theme_light() + 
  xlab(label = 'Date') +
  ggtitle(label = 'Top Hastags Counts') +
  geom_line() + 
  scale_color_manual(values = c('embarazo' = 'green3', 'aborto' = 'red'))

plt %>% ggplotly()


#-------------------------------------------------------------
# NETWORK ANALYSIS

# bigram analysis

# extract bigrams
bi.gram.words <- tweets.df %>% 
  unnest_tokens(
    input = Text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

# glimpse
bi.gram.words %>% 
  select(bigram) %>% 
  head(10)

# filter by common stopwords and remove white spaces
bi.gram.words %<>% 
  separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! word1 %in% stopwords.df$word) %>% 
  filter(! word2 %in% stopwords.df$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) 

# group again and count
bi.gram.count <- bi.gram.words %>% 
  count(word1, word2, sort = TRUE) %>% 
  # We rename the weight column so that the 
  # associated network gets the weights
  rename(weight = n)

# glimpse
bi.gram.count %>% head()

# weight distribution
bi.gram.count %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Distribución de pesos")
# full skewed

# log transformation
bi.gram.count %>% 
  mutate(weight = log(weight + 1)) %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Distribución de pesos (log)")

# network definition:
# - each word is a node
# - two words connected represent a bigram
# - weight of an edge represents n
# - directed or not

# 

threshold <- 200

# scale by a global factor
ScaleWeight <- function(x, lambda) {
  x / lambda
}

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
  graph_from_data_frame(directed = FALSE)

network
is.weighted(network)

# visualization
plot(
  network, 
  vertex.size = 1,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  edge.color = 'gray', 
  main = 'Red de conteo de bigramas', 
  sub = glue('Peso límite: {threshold}'), 
  alpha = 50
)


# additional information
# - sizes of nodes by degree
# - sizes of edges by weight

# Store the degree.
V(network)$degree <- strength(graph = network)

# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

plot(
  network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 2*V(network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(network)$width ,
  main = 'Red de conteo de bigramas', 
  sub = glue('Peso límite: {threshold}'), 
  alpha = 50
)

#---
# biggest connected component
clusters(graph = network)

# Select biggest connected component.  
V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)

cc.network 

# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)

# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)

plot(
  cc.network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 10*V(cc.network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(cc.network)$width ,
  main = 'Componente más grande', 
  sub = glue('Peso límite: {threshold}'), 
  alpha = 50
)

#-------------------------------------------------------------
# INTERACTIVE NETWORK

# Use network D3
# Treshold
threshold <- 80

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)


#-------------------------------------------------------------
# SKIPGRAM ANALYSIS

skip.window <- 2

skip.gram.words <- tweets.df %>% 
  unnest_tokens(
    input = Text, 
    output = skipgram, 
    token = 'skip_ngrams', 
    n = skip.window
  ) %>% 
  filter(! is.na(skipgram))


skip.gram.words$num_words <- skip.gram.words$skipgram %>% 
  map_int(.f = ~ ngram::wordcount(.x))

skip.gram.words %<>% filter(num_words == 2) %>% select(- num_words)

skip.gram.words %<>% 
  separate(col = skipgram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! word1 %in% stopwords.df$word) %>% 
  filter(! word2 %in% stopwords.df$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) 

skip.gram.count <- skip.gram.words  %>% 
  count(word1, word2, sort = TRUE) %>% 
  rename(weight = n)

skip.gram.count %>% head()


# visualization
# Treshold
threshold <- 80

network <-  skip.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Select biggest connected component.  
V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)

# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)
# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = cc.network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(cc.network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(cc.network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)



#-------------------------------------------------------------
# COMMUNITY DETECTION

# using Louvain method
comm.det.obj <- cluster_louvain(
  graph = cc.network, 
  weights = E(cc.network)$weight
)

comm.det.obj
# low modularity due to large sample

# node attribute - zoom and click clusters
V(cc.network)$membership <- membership(comm.det.obj)

# We use the membership label to color the nodes.
network.D3$nodes$Group <- V(cc.network)$membership

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)


# words per cluster
membership.df <- tibble(
  word = V(cc.network) %>% names(),
  cluster = V(cc.network)$membership
)

V(cc.network)$membership %>%
  unique %>% 
  sort %>% 
  map_chr(.f = function(cluster.id) {
    
    membership.df %>% 
      filter(cluster == cluster.id) %>% 
      # Get 15 at most 15 words per cluster.
      slice(1:15) %>% 
      pull(word) %>% 
      str_c(collapse = ', ')
    
  }) 


#-------------------------------------------------------------
# CORRELATION ANALYSIS (PHI COEFFICIENT)

cor.words <- words.df %>% 
  group_by(word) %>% 
  filter(n() > 10) %>% 
  pairwise_cor(item = word, feature = ID)


topic.words <- c('embarazada', 'aborto', 'bebe')


# Set correlation threshold. 
threshold = 0.1

network <- cor.words %>%
  rename(weight = correlation) %>% 
  # filter for relevant nodes.
  filter((item1 %in% topic.words | item2 %in% topic.words)) %>% 
  filter(weight > threshold) %>%
  graph_from_data_frame()

V(network)$degree <- strength(graph = network)

E(network)$width <- E(network)$weight/max(E(network)$weight)

network.D3 <- igraph_to_networkD3(g = network)

network.D3$nodes %<>% mutate(Degree = 5*V(network)$degree)

# Define color groups. 
network.D3$nodes$Group <- network.D3$nodes$name %>% 
  as.character() %>% 
  map_dbl(.f = function(name) {
    index <- which(name == topic.words) 
    ifelse(
      test = length(index) > 0,
      yes = index, 
      no = 0
    )
  }
  )

network.D3$links %<>% mutate(Width = 10*E(network)$width)

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  # We color the nodes using JavaScript code.
  colourScale = JS('d3.scaleOrdinal().domain([0,1,2]).range(["gray", "blue", "red", "black"])'), 
  opacity = 0.8,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We define edge properties using JavaScript code.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  linkDistance = JS("function(d) { return 550/(d.value + 1); }"), 
  fontSize = 18,
  zoom = TRUE, 
  opacityNoHover = 1
)



# sentiment
# get_nrc_sentiment from syuzhet
tweets.vector <- as.vector(tweets.df$Text)

emocion.df <- get_nrc_sentiment(char_v = tweets.vector, language = "spanish")
# this method is too slow

#-------------------------------------------------------------
# ACCOUNTS NETWORTK

# need retweet_screen_name for this in database



#-------------------------------------------------------------
# CLUSTER ANALYSIS

# use tdm created previously for distance matrix
quanteda_corpus <- quanteda::corpus(tweets.df$Text)

# Document frequencies matrix
dfmat <- dfm(corpus_subset(quanteda_corpus), verbose = T)

# Distance matrix using cosine
#cosine.matrix <- as.matrix( textstat_simil(dfmat, method = "cosine", margin = "documents") )
# too expensive


gc()


