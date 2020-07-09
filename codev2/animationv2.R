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


word_count <- getCount(tweets.df,'condon')
word_count$Created_At_Round <- substr(word_count$Created_At_Round, 0, 4)

word_count <- word_count %>% 
  group_by(Created_At_Round) %>% 
  summarise(wcount = sum(wcount))

word_count[is.na(word_count)] <- 0

# Plot
animation2 <-  word_count %>%
  ggplot( aes(x= as.numeric(Created_At_Round), y = wcount)) +
  geom_line(colour = "black") +
  scale_x_continuous(limits = c(2006,2020),) +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1,1), guide = FALSE) +
  geom_point(shape = 21, colour = "black", aes(fill = wcount), size = 5, stroke = 1) +
  ggtitle("Evolución de la palabra 'condon'") +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  ylab("Frecuencia") +
  xlab("Años") +
  transition_reveal(as.numeric(Created_At_Round))


animation

 

anim_save("datamarkdownv2\\animation2.gif", animation2)


