#Some defection


library(webshot)
library(htmlwidgets)
library(knitr)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(stringr)
library(gridExtra)
library(scales)
library(lubridate)
library(ggrepel)
library(reshape2)
library(kableExtra)
library(tm)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(textdata)
library(broom)
#library(topicmodels)
library(bit64)

webshot::install_phantomjs()
#Load the data
#setwd("C:\\Users\\lzm11\\OneDrive\\course\\bios611\\bios611_course_project")

tweets16 <- timetk::tk_tbl(data.table::fread("source_data/ttweets1.csv", encoding= "UTF-8"))
tweets20 <- timetk::tk_tbl(data.table::fread("source_data/ttweets2.csv", encoding= "UTF-8"))

print("read complete")
#date formatting and cleaning
#I only need tweets from  07.18 to 10.17 in 2016 data
tweets16$date <- mdy_hm(tweets16$date)
tweets16 <- tweets16 %>% filter(date >= "2016-07-18 00:00:00" & date <= "2016-10-18 00:00:00")
tweets20$date <- mdy_hm(tweets20$date)
glimpse(tweets16)


#text mining section

print("show the content of text")
#kable(head(tweets16 %>% select(text), 20), format = "html") %>%
  #kable_styling() %>%
  #column_spec(1, width = "19cm")

#kable(head(tweets20 %>% select(text), 20), format = "html") %>%
  #kable_styling() %>%
  #column_spec(1, width = "19cm")

print("clean the line end, amp, url and icon")
tweets16$text <- str_replace_all(tweets16$text, "[\n]" , "") 
tweets16$text <- str_replace_all(tweets16$text, "&amp", "") 
tweets16$text <- str_replace_all(tweets16$text, "http.*" , "")
tweets16$text <- iconv(tweets16$text, "latin1", "ASCII", sub="")

tweets20$text <- str_replace_all(tweets20$text, "[\n]" , "") 
tweets20$text <- str_replace_all(tweets20$text, "&amp", "") 
tweets20$text <- str_replace_all(tweets20$text, "http.*" , "")
tweets20$text <- iconv(tweets20$text, "latin1", "ASCII", sub="")


#build the corpus
#vcorpus strictly requires the column name to be coordinate, so change 2 name
tweets16 <- tweets16 %>% rename (doc_id = id)
tweets16 <- tweets16 %>% rename (time = date)

tweets20 <- tweets20 %>% rename (doc_id = id)
tweets20 <- tweets20 %>% rename (time = date)

Corpus16 <- DataframeSource(tweets16)
Corpus16 <- VCorpus(Corpus16)
Corpus20 <- DataframeSource(tweets20)
Corpus20 <- VCorpus(Corpus20)

#inspect the corpus
content(Corpus16[[1]])
content(Corpus20[[1]])

#clean the corpus
#upper2lower, remove num, stopwords, punc, and strip
CleanCorpus <- function(x){
  x <- tm_map(x, content_transformer(tolower))
  x <- tm_map(x, removeNumbers) #remove numbers before removing words. Otherwise "trump2016" leaves "trump"
  x <- tm_map(x, removeWords, tidytext::stop_words$word)
  x <- tm_map(x, removePunctuation)
  x <- tm_map(x, stripWhitespace)
  return(x)
}


#remove the name
#"sleepy" is also removed because of "sleepy joe"
RemoveNames <- function(x) {
  x <- tm_map(x, removeWords, c("sleepy", "donald", "hillary", "clinton", "trump", "realdonaldtrump", "hillaryclinton", "joe", "biden", "joebiden"))
  return(x)
}

CreateTermsMatrix <- function(x) {
  x <- TermDocumentMatrix(x)
  x <- as.matrix(x)
  y <- rowSums(x)
  y <- sort(y, decreasing=TRUE)
  return(y)
}

#conduct the cleaning
#not remove the name
Corpus16 <- CleanCorpus(Corpus16)
TermFreq16 <- CreateTermsMatrix(Corpus16)
Corpus20 <- CleanCorpus(Corpus20)
TermFreq20 <- CreateTermsMatrix(Corpus20)

#show the content
content(Corpus16[[1]])
content(Corpus20[[1]])

#create directory for figures
dir.create("./figures/",showWarnings=FALSE);

#top 20 words histogram
DF16_top20 <- data.frame(word=names(TermFreq16), count=TermFreq16)

t1 <- DF16_top20[1:20,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  ggtitle("Word Frequency in 2016") +
  geom_bar(stat='identity', fill="red") + coord_flip() + theme(legend.position = "none") +
  labs(x="")

DF20_top20 <- data.frame(word=names(TermFreq20), count=TermFreq20)

t2 <- DF20_top20[1:20,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  ggtitle("Word Frequency in 2020") +
  geom_bar(stat='identity', fill="green") + coord_flip() + theme(legend.position = "none") +
  labs(x="")

g <- grid.arrange(t1, t2, nrow=1)
ggsave("./figures/top20.png", g)

#word cloud remove name
set.seed(1234)

Corpus16_1 <- RemoveNames(Corpus16)
TermFreq16 <- CreateTermsMatrix(Corpus16_1)
DF16_cloud <- data.frame(word=names(TermFreq16), count=TermFreq16)

Corpus20_1 <- RemoveNames(Corpus20)
TermFreq20 <- CreateTermsMatrix(Corpus20_1)
DF20_cloud <- data.frame(word=names(TermFreq20), count=TermFreq20)

w1 <- wordcloud2::wordcloud2(DF16_cloud[1:100,], color = "random-light", backgroundColor = "white", shuffle=FALSE, size=0.4)
withr::with_dir('figures', saveWidget(w1, file="tmp.html",selfcontained=FALSE))
webshot("./figures/tmp.html","./figures/wordcloud1.png", delay =5, vwidth = 480, vheight=480)
w2 <- wordcloud2::wordcloud2(DF20_cloud[1:100,], color = "random-light", backgroundColor = "white", shuffle=FALSE, size=0.4)
withr::with_dir('figures', saveWidget(w1, file="tmp.html",selfcontained=FALSE))
webshot("./figures/tmp.html","./figures/wordcloud2.png", delay =5, vwidth = 480, vheight=480)

#tidytext
#we need to break the corpus list to show bigramms
Tidy16 <- tidy(Corpus16)
Tidy16_1 <- tidy(Corpus16_1) #without names
Tidy20 <- tidy(Corpus20)
Tidy20_1 <- tidy(Corpus20_1) #without names

#bigramms
plotBigrams <- function(tibble, topN=20, title="", color="#FF1493"){
  x <- tibble %>% select(text) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  y <- x %>% count(bigram, sort = TRUE) %>% top_n(topN, wt=n) %>%
    ggplot(aes(x=reorder(bigram, n), y=n)) +
    geom_bar(stat='identity', fill=color) + coord_flip() +
    theme(legend.position="none") + labs(x="", title=title)
}

b1 <- plotBigrams(Tidy16, title="With names 2016", color="red")
b2 <- plotBigrams(Tidy16_1, title="Without names 2016", color="red")


b3 <- plotBigrams(Tidy20, title="With names 2020", color="green")
b4 <- plotBigrams(Tidy20_1, title="Without names 2020", color="green")

g <- grid.arrange(b1, b2, b3, b4, ncol=2)
ggsave("./figures/bigramms.png", g, width = 800/72, height = 600/72, dpi = 150)

#sentiment analysis
get_sentiments("bing")

DocMeta16_1 <- meta(Corpus16_1)
DocMeta16_1$date <- date(DocMeta16_1$time)
Tidy16_1$date <- DocMeta16_1$date

DocMeta20_1 <- meta(Corpus20_1)
DocMeta20_1$date <- date(DocMeta20_1$time)
Tidy20_1$date <- DocMeta20_1$date

NoNamesTidy <- bind_rows(Tidy16_1, Tidy20_1)
Words <- NoNamesTidy %>% unnest_tokens(word, text)
Bing <- Words %>% inner_join(get_sentiments("bing"), by="word")

b1 <- Bing %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="number of times used", title="Donald Trump's most used words in 2016") +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))
b2 <- Bing %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="number of times used", title="Donald Trump's most used words in 2020") +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))

g <- grid.arrange(b1, b2)
ggsave("./figures/mostused.png", g, width = 800/72, height = 600/72, dpi = 150)

#time series
t1 <- Bing %>% group_by(date) %>% count(sentiment) %>%
  spread(sentiment, n) %>% mutate(score=positive-negative) %>%
  ggplot(aes(x=date, y=score)) +
  scale_x_date(limits=c(as.Date("2016-07-18"), as.Date("2016-10-07")), date_breaks = "1 month", date_labels = "%b") +
  geom_line(stat="identity", col="blue") + geom_smooth(col="red") + labs(title="Sentiment 2016")

t2 <- Bing %>% group_by(date) %>% count(sentiment) %>%
  spread(sentiment, n) %>% mutate(score=positive-negative) %>%
  ggplot(aes(x=date, y=score)) +
  scale_x_date(limits=c(as.Date("2020-07-18"), as.Date("2020-10-07")), date_breaks = "1 month", date_labels = "%b") +
  geom_line(stat="identity", col="blue") + geom_smooth(col="red") + labs(title="Sentiment 2020")

g <- grid.arrange(t1, t2, ncol=1)
ggsave("./figures/time_bing.png", g, width = 800/72, height = 600/72, dpi = 150)

#sentiment analysis afinn
get_sentiments("afinn")

Afinn <- Words %>% inner_join(get_sentiments("afinn"), by="word")

a1 <- Afinn %>%  group_by(date) %>% summarise(value=sum(value)) %>%
  ggplot(aes(x=date, y=value)) +
  scale_x_date(limits=c(as.Date("2016-07-18"), as.Date("2016-10-07")), date_breaks = "1 month", date_labels = "%b") +
  geom_line(stat="identity", col="blue") + geom_smooth(col="red") + labs(title="Sentiment 2016")

a2 <- Afinn %>% group_by(date) %>% summarise(value=sum(value)) %>%
  ggplot(aes(x=date, y=value)) +
  scale_x_date(limits=c(as.Date("2020-07-18"), as.Date("2020-10-07")), date_breaks = "1 month", date_labels = "%b") +
  geom_line(stat="identity", col="blue") + geom_smooth(col="red") + labs(title="Sentiment 2018")

g <- grid.arrange(a1, a2)
ggsave("./figures/time_affin.png", g, width = 800/72, height = 600/72, dpi = 150)

#sentiment nrc
get_sentiments("nrc")

Nrc <- Words %>% inner_join(get_sentiments("nrc"), by="word")

n1 <- Nrc %>% filter(date <= "2017-02-04") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=500)) +
  labs(x="", y="", title="2016")
n2 <- Nrc %>% filter(date > "2017-02-04") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=500)) +
  labs(x="", y="", title="2020")
g <- grid.arrange(n1, n2, nrow=1)
ggsave("./figures/nrc.png", g, width = 800/72, height = 600/72, dpi = 150)


