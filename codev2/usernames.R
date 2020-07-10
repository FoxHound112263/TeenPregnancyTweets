# POPULAR ACCOUNTS

likes <-  aggregate(likes ~ screen_name, tweets.df, sum)
#likes <- likes[order(likes$likes,decreasing = T),]

library(gghighlight)

# bar plot
likesplt <- likes %>% 
  # Set count threshold. 
  filter(likes > 1300) %>%
  mutate(screen_name = reorder(screen_name, likes)) %>%
  ggplot(aes(x = screen_name, y = likes)) +
  theme_light() + 
  geom_col(fill = 'black', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  gghighlight(likes == 3568) +
  ggtitle(label = "'likes'")

likesplt %>% ggplotly()

#save(likesplt, file = "datamarkdownv2\\likes.RData")



# Retweets

retweets <-  aggregate(retweets ~ screen_name, tweets.df, sum)

# bar plot
retweetsplt <- retweets %>% 
  # Set count threshold. 
  filter(retweets > 635) %>%
  mutate(screen_name = reorder(screen_name, retweets)) %>%
  ggplot(aes(x = screen_name, y = retweets)) +
  theme_light() + 
  geom_col(fill = 'black', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  gghighlight(retweets == 2087) +
  ggtitle(label = "'Usuarios con más likes y retweets'")

retweetsplt %>% ggplotly()

save(retweetsplt, file = "datamarkdownv2\\retweetsplt.RData")



popular <- subplot(likesplt, retweetsplt,margin = 0.1,titleX = T,titleY = T)

popular 

save(popular, file = "datamarkdownv2\\popular.RData")



# Similarity between users



library(rtweet)
series <- ts_plot(tweets.df, by="days")
series
