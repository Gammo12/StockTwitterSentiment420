#Project #4 Stock sentiment analysis on Twitter for gainers and losers on a single day
#Load Libraries
library(twitteR)
library(ROAuth)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)

setwd(dir)
load(file = "twitter_credentials.RData")
my_oauth <- setup_twitter_oauth(t.credentials$consumerKey,t.credentials$consumerSecret,t.credentials$oauthKey,
                                t.credentials$oauthSecret)

# Gainers
positive_tru <- searchTwitter("$TRU",
                              n=100,
                              since="2018-04-20",
                              until = "2018-04-20")
positive_eric <- searchTwitter("$ERIC",
                               n=100,
                               since="2018-04-20",
                               until = "2018-04-20")
positive_pf <- searchTwitter("$PF",
                             n=100,
                             since="2018-04-20",
                             until = "2018-04-20")
#Losers
negative_sage <- searchTwitter("$SAGE",
                               n=100,
                               since="2018-04-20",
                               until = "2018-04-20")
negative_skx <- searchTwitter("$SKX",
                              n=100,
                              since="2018-04-20",
                              until = "2018-04-20")
negative_man <- searchTwitter("$GNTX",
                              n=100,
                              since="2018-04-20",
                              until = "2018-04-20")

positive_tweets <- c(positive_tru, positive_eric,positive_pf)
negative_tweets <- c(negative_sage,negative_skx,negative_man)
#save("positive_tweets", file="Positive.Tweets.final")
#save("negative_tweets", file="Negative.Tweets.final")
#load(file = "Positive.Tweets.final")
#load(file = "Negative.Tweets.final")

# Extract Text to Corpus
txt_corpus <- function(tweets){
  adj_text <- lapply(tweets,
                     function(t) {iconv(t$getText(),
                                        "latin1", "ASCII", sub="")})
  corpus <- Corpus(VectorSource(adj_text))
}

data.corpus1 <- txt_corpus(positive_tweets)
data.corpus2 <- txt_corpus(negative_tweets)

# Preprocessing function
preprocess <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  removeURL <- function(x) {
    gsub(" ?(f|ht)tp(s?)://(.*)", "", x)
  }
  
  corpus <- tm_map(corpus,content_transformer(removeURL))
  corpus <- tm_map(corpus,content_transformer(removePunctuation))
  
  english.stopwords <- stopwords("english")
  corpus <- tm_map(corpus,content_transformer(removeWords),
                   english.stopwords)
  
  removeNumberWords <- function(x) {
    gsub("([[:digit:]]+)([[:alnum:]])*", "", x)
  }
  
  corpus <- tm_map(corpus,content_transformer(removeNumberWords))
  corpus <- tm_map(corpus,content_transformer(stemDocument))
  corpus <- tm_map(corpus,content_transformer(stripWhitespace))
}

data.corpus1 <- preprocess(data.corpus1)
data.corpus2 <- preprocess(data.corpus2)
#save("data.corpus1",file="Positive.Corpus.final")
#save("data.corpus2",file="Negative.Corpus.final")
#load(file = "Positive.Corpus.final")
#load(file = "Negative.Corpus.final")

# Create TDM 
## Positive
tdm1 <- TermDocumentMatrix(data.corpus1)
#save("tdm1", file="Positive.final")
#load(file = "Positive.final")
inspect(tdm1)

## Negative
tdm2 <- TermDocumentMatrix(data.corpus2)
#save("tdm2", file="Negative.final")
#load(file = "Negative.final")
inspect(tdm2)

# Word Frequencies
## Positive
pos_matrix <- as.matrix(tdm1)
pos_wordfreq <- rowSums(pos_matrix)
pos_sortedfreq <- sort(pos_wordfreq, decreasing = TRUE)
head(pos_sortedfreq)

## Negative
neg_matrix <- as.matrix(tdm2)
neg_wordfreq <- rowSums(neg_matrix)
neg_sortedfreq <- sort(neg_wordfreq, decreasing = TRUE)
head(neg_sortedfreq)

# WordClouds
## Positive
pal.choice <- brewer.pal(8, "Dark2")
set.seed(34)
wordcloud(words=names(pos_sortedfreq),
          freq = pos_sortedfreq,
          min.freq = 8,
          random.order = FALSE,
          colors = pal.choice)

## Negative
pal.choice <- brewer.pal(8, "Dark2")
set.seed(35)
wordcloud(words=names(neg_sortedfreq),
          freq = neg_sortedfreq,
          min.freq = 8,
          random.order = FALSE,
          colors = pal.choice)

# Sentiment Analysis Begin
pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')

sentiment.na <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p == 0 & n == 0)
    return (NA)
  else
    return (p - n)
}

#Pull just text for  word freq
just_txt <- function(tweet){
  lapply(tweet,
         function(t) {iconv(t$getText(),
                            "latin1", "ASCII", sub="")})
}

positive_txt <- just_txt(positive_tweets)
negative_txt <- just_txt(negative_tweets)

pos_scores <- sapply(positive_txt, 
                     sentiment.na,
                     pos.words, 
                     neg.words)

neg_scores <- sapply(negative_txt,
                     sentiment.na,
                     pos.words,
                     neg.words)

# Dataframes of scores and Frequencies
pos_df <- data.frame(table(pos_scores))
neg_df <- data.frame(table(neg_scores))

#Visualize the results
library(gridExtra)
pos.score.num <- as.numeric(levels(pos_df$pos_scores))
mean_num <- median(pos_scores, na.rm = TRUE)
pos.plot <- ggplot(pos_df, aes(x=pos.score.num, y=pos_df$Freq)) +
  geom_bar(stat = "identity") + 
  geom_vline(data=pos_df, aes(xintercept= mean_num, colour="red", show.legend=FALSE), size=1.25)
pos.plot.t <- pos.plot + ggtitle("Gaining Stock Sentiment Scores") +
  xlab("Sentiment scores") +
  ylab("Frequency") 

neg.score.num <- as.numeric(levels(neg_df$neg_scores))
mean_num2 <- median(neg_scores, na.rm = TRUE)
neg.plot <- ggplot(neg_df, aes(x=neg.score.num, y=neg_df$Freq)) +
  geom_bar(stat = "identity") +
  geom_vline(data=neg_df, aes(xintercept = mean_num2, colour="red", show.legend=FALSE), size=1.25)

neg.plot.t <- neg.plot + ggtitle("Losing Stock Sentiment Scores") +
  xlab("Sentiment scores") +
  ylab("Frequency") 

grid.arrange(pos.plot.t,neg.plot.t, ncol=2)

#Create Dataframe with Tweets and Sentiment Score
df_func <- function(tweets,sentiment){
  df <- data.frame(Tweets=matrix(unlist(tweets), nrow=300, byrow=T),
                   Sentiment_Score=sentiment,stringsAsFactors=FALSE)
}

df_pos <- df_func(positive_txt, pos_scores)
df_neg <- df_func(negative_txt,neg_scores)
new_df <- rbind(df_pos, df_neg)
new_df$Tweets <- gsub("([[:digit:]]+)([[:alnum:]])*", "", new_df$Tweets)
new_df$Tweets <- gsub()
new_df$Sentiment_Score[is.na(new_df$Sentiment_Score)] <- 0

new_df2 <- rbind(new_df[15:17,],new_df[110:112,],new_df[210:212,],
                 new_df[420:422,],new_df[510:512,],new_df[310:312,])
new_df2
#write.csv(new_df2, file = "Tweet_dfsample") #To have a clean looking tble to put in report