# Animation try
library(hrbrthemes)
library(gganimate)
library(ggplot2)
library(gifski)

getCount <- function(data,keyword)
{
  wcount <- str_count(tweets.df$Text, keyword)
  return(data.frame(data,wcount))
}


word_count <- getCount(tweets.df,'embarazo')
word_count$Created_At_Round <- substr(word_count$Created_At_Round, 0, 4)

word_count <- word_count %>% 
  group_by(Created_At_Round) %>% 
  summarise(wcount = sum(wcount))

word_count[is.na(word_count)] <- 0

# Plot
animation <-  word_count %>%
  ggplot( aes(x= as.numeric(Created_At_Round), y = wcount)) +
  geom_line(color = "blue") +
  geom_point() +
  ggtitle("Evolución de la palabra 'embarazo'") +
  theme_ipsum() +
  ylab("Frecuencia") +
  xlab("Años") +
  transition_reveal(as.numeric(Created_At_Round))


 animation

 
 save(animation, file = "datamarkdown\\animation.RData")
 