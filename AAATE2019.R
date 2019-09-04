#Twitter #AAATE2019
#===========================================================================
#What did we twitter about during the AAATE conference? 
#A search for our hottest topics during this conference!
#@sil_aarts
#===========================================================================

#Install packages
install.packages("rtweet")
install.packages("viridis")
install.packages("harrypotter")
install.packages("LaCroixColor")
install.packages("igraph")
install.packages("ggraph")
install.packages("stopwords")

if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

#Load libraries
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(harrypotter)
library(igraph)
library(ggraph)
library(stopwords)
library(tidyr)

#Authentication using Key
token <- create_token(
  app = "Twitter_AAATE2019",
  consumer_key = "SFwKpMJA3PidmKa98N3jOibEJ",
  consumer_secret = "eWZ9ukULmR7FyNPUQUnS95Jgd8K9t6NwfcM3Ubdoduar210Lyd")

#Get tweets from #AAATE 2019
tweets <- search_tweets(q = "#AAATE2019", n=1500)

#Check unique twitterers: n=198
unique(tweets$screen_name)
#Recheck using ID codes
unique(tweets$user_id)

#Order favourite tweets
#KirseRose provided the most liked tweet: 34 likes: activity trackers for people with weelchairs. 21 retweets.
#DavidHobbs08 31 likes: goodbye tweet.

#Mean # of tokens in a tweet: 139
tweets %>%
  summarise(mean_width= mean(display_text_width))

#Select col with tweet text
tweets1 <- tweets %>%
  select("text")

#One col, one word
#Change colnames
colnames(tweets1) <- c("woord")

#Every word one vector
tweets2 <- tweets1 %>% 
  unnest_tokens(word, woord) 

#Delete stopwords
tweets3 <- tweets2 %>% 
  anti_join(get_stopwords(language="en", source="snowball"))

#Frequency words
tweets4 <- tweets3 %>%
  count(word, sort=T)

#Delete input 2 and 3: regarding https: non informative
tweets5 <- tweets4 %>%
  filter(n != "988")
#First 50 rows
tweets6 <- tweets5 %>%
  head(25)

#Plot 1
#GGpplot: bar chart, filter (n>50), flipped
#Make some colors for the bars: Harry Potter
colors <-hp(25, option = "LunaLovegood")
#GGplot: bar 
p1 <- tweets6 %>% 
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill=colors)+
  ggtitle(label = "Which words did you use while tweeting during #AAATE2019?", subtitle="Recognize some of your words?  Insight in word frequency.")+ 
  xlab("")+ ylab("")+labs(caption="Source: Twitter | #AAATE2019, Plot by @sil_aarts")+
  scale_y_continuous(breaks=seq(0, 1400, 100))+
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="mono"),
        plot.background = element_rect(fill = "black"),
        panel.grid=element_blank(),
        plot.title = element_text(size=20, color="white", face="bold", hjust=0),
        plot.subtitle=element_text(size=14, color="white"),
        plot.caption= element_text(size=10, hjust=1, color="white"),
        axis.text.x= element_text(size=12,face='bold', colour='white'),
        axis.text.y= element_text(size=12,face='bold', colour='white'),
        axis.title.x = element_text(color = "white", size = 12, angle = 0, hjust = 0.5, vjust = 1),
        axis.title.y = element_blank())+
  coord_flip()

#Run it!
p1

#Plot 2
#Every vector, two words, adjacent 
tweets_bi1 <- tweets1 %>% 
  unnest_tokens(word, woord,token = "ngrams", n = 2)

#Delete stopwords
tweets_bi2 <- tweets_bi1 %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords("english"),
         !word2 %in% stopwords("english")) %>%
  count(word1, word2, sort = TRUE)

#Delete som rows manually: errors in tweets?
tweets_bi3 <- tweets_bi2[-c(1, 2, 4, 7, 24), ]

#GGraph 
tweets_bi4 <- tweets_bi3 %>%
  filter(n > 20) %>%
  graph_from_data_frame()

#Bigrams plot
p2 <- ggraph(tweets_bi4, layout="linear")+
  geom_edge_arc(aes(colour = n), edge_colour="yellow")+
  geom_node_text(aes(label=name), vjust=1, hjust=0.8, size=5, colour="white")+
  ggtitle(label = "Word combinations in our tweets during #AAATE2019", subtitle= "Lines are indicative for combinations of words. The combination 'assistive technology' is the boldest line,\nmaking it the most used word combination in our tweets.")+ 
  labs(caption="Source: Twitter | #AAATE2019, Plot by @sil_aarts")+
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="mono"),
        plot.background = element_rect(fill = "black"),
        panel.grid=element_blank(),
        plot.title = element_text(size=20, color="white", face="bold", hjust=0),
        plot.subtitle = element_text(size=14, color="white", face="italic", hjust=0),
        plot.caption= element_text(size=10, hjust=1, color="white"),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_flip()

#Run it!
p2

#Plot 3
#GGraph nodes
set.seed(1609)
x <- grid::arrow(type="closed", length= unit(.35, "inches"))
p3 <- ggraph(tweets_bi4, layout="fr")+
  geom_edge_link(aes(edge_alpha=n), show.legend = F, arrow=x, end_cap=circle(0.10, "inches"), edge_colour="white")+
  geom_node_point(color="deeppink2", size=3)+
  geom_node_text(aes(label=name), vjust=1, hjust=0.8, size=5, colour="white")+
  ggtitle(label = "Word combinations in our tweets during #AAATE2019", subtitle= "Arrows are indicative for combinations of words. The combination 'assistive technology' is the boldest arrow,\nmaking it the most used word combination in our tweets.")+ 
  labs(caption="Source: Twitter | #AAATE2019, Plot by @sil_aarts")+
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="mono"),
        plot.background = element_rect(fill = "black"),
        panel.grid=element_blank(),
        plot.title = element_text(size=20, color="white", face="bold", hjust=0),
        plot.subtitle=element_text(size=14, color="white"),
        plot.caption= element_text(size=10, hjust=1, color="white"),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#Run it!
p3
