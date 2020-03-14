
---
  title: "Twitter_coronavirusnederland"
author: "Sil Aarts"
date: "3/14/2020"
---

#Load libraries
library(rtweet)
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(stopwords)
library(tidyr)
library(extrafont)

#Authentication using key: create an account before
twitter_token <- create_token(
  app = "Corona NL",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "")

#Get tweets from #AAATE 2019
tweets1 <- search_tweets(q = "#coronavirusnederland", n=10000)

#Time 08.44
#Check unique twitterers: n=6617
names <- unique(tweets1$screen_name)
#Recheck using ID codes
id <- unique(tweets1$user_id)

#Mean # of tokens in a tweet: 146
mean <- tweets1 %>%
  summarise(mean_width= mean(display_text_width))

#Select col with tweet text
tweets_text <- tweets1 %>%
  select("text")

#One col, one word
#Change colnames
colnames(tweets_text) <- c("woord")

#Every word one vector
tweets_text2 <- tweets_text %>% 
  unnest_tokens(word, woord) 

#Delete stopwords
tweets_text3 <- tweets_text2 %>% 
  anti_join(get_stopwords(language="nl", source="snowball"))%>%
  anti_join(get_stopwords(language="en", source="snowball"))

#Frequency words
tweets_text4 <- tweets_text3 %>%
  count(word, sort=T)

#Delete input 2 and 3: regarding https: non informative
tweets_text5 <- tweets_text4 %>%
  slice(-2,-3)

#Delete all input with numbers & two extra entries because of content
tweets_text6 <- tweets_text5 %>% 
  filter(is.na(as.numeric(word)))%>%
  slice(-45, -50)

#First 50 rows
tweets_text7 <- tweets_text6 %>%
  head(50)

#Plot 1
#GGpplot: bar chart, filter (n>50), flipped
p1 <- tweets_text7 %>% 
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill="white")+
  labs(title = "Top 50 meest gebruikte woorden in #coronavirusnederland",
          caption= "Source: twitter #coronavirusnederland | Plot by @sil_aarts")+ 
  xlab("")+ ylab("")+
  scale_y_continuous(breaks=seq(0, 7000, 1000))+
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="Georgia"),
        plot.background = element_rect(fill = "gray30"),
        panel.grid=element_blank(),
        plot.title = element_text(size=18, color="white", face="bold", hjust=0),
        plot.caption = element_text(size=12, color="darkgrey", face="bold", hjust=1),
        axis.text.x= element_text(size=11,face='bold', colour='black'),
        axis.text.y= element_text(size=11, colour='black'),
        axis.title.x = element_text(color = "white", size = 15, angle = 0, hjust = 0.5, vjust = 1),
        axis.title.y = element_blank())+
  coord_flip()

#Run it!
p1

#Plot 2
#Every vector, two words, adjacent 
tweets_bi1 <- tweets_text %>% 
  unnest_tokens(word, woord,token = "ngrams", n = 2)

#Delete stopwords
tweets_bi2 <- tweets_bi1 %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords("dutch"),
         !word2 %in% stopwords("dutch")) %>%
  count(word1, word2, sort = TRUE)

#Delete the word: t.co & https > non-informative
tweets_bi3 <- tweets_bi2 %>%
  filter(word1 != "https" & word2 != "https" & word1 != "t.co" & word2 != "t.co")

#Get ready for the GGraph 
tweets_bi4 <- tweets_bi3 %>%
  filter(n > 300) %>%
  graph_from_data_frame()

#Plot 2
#GGraph nodes
set.seed(1609)
x <- grid::arrow(type="closed", length= unit(.35, "inches"))
p2 <- ggraph(tweets_bi4, layout="fr")+
  geom_edge_link(aes(edge_alpha=n), show.legend = F, arrow=x, end_cap=circle(0.10, "inches"), edge_colour="white")+
  geom_node_point(color="darkgoldenrod", size=3)+
  geom_node_text(aes(label=name), vjust=1, hjust=0.8, size=5, colour="black")+
  labs(title = "<br><br>Woordcombinaties met hashtag #coronavirusnederland", 
        subtitle= "Combinaties die vaker dan 300x voorkomen.<br>De pijlen duiden op relaties tussen woorden; hoe witter de pijl, hoe vaker de combinatie voorkomt.",
  caption= "Source: twitter #coronavirusnederland | Plot by @sil_aarts")+
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="Georgia"),
        plot.background = element_rect(fill = "gray30"),
        panel.grid=element_blank(),
        plot.title = element_markdown(size=18, color="white", face="bold", hjust=0.5),
        plot.subtitle=element_markdown(size=12, color="white", hjust=0.5),
        plot.caption= element_text(size=12, hjust=1, color="darkgrey"),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#Run it!
p2
