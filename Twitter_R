#Code for Barchart Twitter acount

#Load libary
library(dplyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(stopwords)
library(quanteda)
library(tm)
library(lubridate)
library(readr)
library(tidytext)
library(stringr)
library(wesanderson)

#Read file
tweets_sil <- read_csv("Desktop/tweets_june2019.csv")

#Bind rows
tweets <- bind_rows(tweets_sil %>%
      mutate(person="Sil")) %>%
      mutate(timestamp=ymd_hms(timestamp))

#Remove weird tokens in the tweets!
remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

#Check frequency of words
frequency <- tidy_tweets %>%
  count(word, sort=T) 

#Delete stopwords in two languages: EN and NL!
tweets_freq <- frequency %>% 
  anti_join(get_stopwords(language="nl", source="snowball")) %>% 
  anti_join(get_stopwords(language="en", source="snowball"))

#Make some colours (n=44 for # of rows)
colours <- wes_palette("Rushmore1", 44, type = "continuous")

#GGplot met columns x=words. Select top & turn chart!
p1 <- tweets_freq %>%
   filter(n > 29) %>%
   mutate(word=reorder(word,n))%>%
ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  geom_bar(stat = "identity", fill=colours)+
  
  annotate(geom="label", x="@fontysfph", y = 85, label = "Dutch scientists\ncurator account", family="Courier", vjust=0.1, size=3.5, fontface="bold")+
  annotate("curve", x="weer", xend = "@nlwetenschap", y = 85, yend = 81 ,size=0.5, arrow=arrow(length=unit(.2, "cm")), colour="white")+
  
  annotate(geom="label", x="research", y = 85, label = "Some of my twitter R buddies", family="Courier", vjust=0.1, size=3.5, fontface="bold")+
  annotate("curve", x="week", xend = "@oscarb123", y = 65, yend = 41, size=0.5, arrow=arrow(length=unit(.2, "cm")), colour="white")+
  annotate("curve", x="week", xend = "@benmoretti", y = 65, yend = 37, size=0.5, arrow=arrow(length=unit(.2, "cm")), colour="white")+
  
  annotate(geom="label", x="#r", y = 75, label = "#TidyTuesday", family="Courier", vjust=0.1, size=3.5, fontface="bold")+
  annotate("curve", x="mi", xend = "#tidytuesday", y = 75, yend = 50 ,size=0.5, arrow=arrow(length=unit(.2, "cm")), colour="white")+
  
  ggtitle(label = "What did I twitter about and @who?", subtitle="Are you somewhere in there?")+
  xlab("Words")+ ylab("Number")+labs(caption="Source: @twitter | With help from: Text Mining with R | Plot by @sil_aarts")+
  theme_minimal()+
  theme(
        plot.background = element_rect(fill = "black"),
        panel.grid=element_blank(),
        plot.title = element_text(size=18, color="white", face="bold", hjust=0),
        plot.subtitle=element_text(size=16, color="white"),
        plot.caption= element_text(size=10, hjust=1, color="white"),
        axis.text.x= element_text(size=9,face='bold', colour='white'),
        axis.text.y= element_text(size=9,face='bold', colour='white'),
        axis.title.x = element_text(color = "white", size = 16, angle = 0, hjust = 0.5, vjust = 1),
        axis.title.y = element_blank())+                    
  coord_flip()

#Run it!
p1
