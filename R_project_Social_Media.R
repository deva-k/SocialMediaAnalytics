# You can collect the company tweets  (e.g. using timelines) to see 
# to which customer contact points they actually reply.
# (do they reply to people?
# Do they reply to people who did not mention the company?
# Do they reply to positive tweets as well or only to negative ones?...)
# Try to get all tweets from 2019.

# package rtweet
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")

# Credentials
# Keys removed from code for security purposes.
twitter_token <- create_token(
  app = "",
  consumer_key = "" ,
  consumer_secret = "",
  access_token = "",
  access_secret = "",
  set_renv=FALSE)


# Look for 500 tweets on #AmericanAir basis without retweets
#rtweets <- rtweet::search_tweets(q = "#AmericanAirlines",
 #                                n = 500,include_rts = FALSE)

# Read csv for tweets extracted with search::fullarchive
Tweets_0302 <- read.csv(file = "C:\\Users\\dkalapati\\Desktop\\SMA\\AA_hist.csv", stringsAsFactors = F)


#tweets <- search_fullarchive(q = "#AmericanAirlines", n = 200,env_name="Class",fromDate="202001010000",toDate="202001280000")

# Add latitude and longitude when possible
Tweets_0302 <- lat_lng(Tweets_0302)

# Install maps package
if(!require("maps")) install.packages("maps"); library("maps")

## Make a map of the world with country boundaries
par(mar = c(0, 0, 0, 0))
maps::map("world", lwd = .25)

## Plot lat and lng points onto world map
points(Tweets_0302$lng, Tweets_0302$lat, pch = 20, cex = 1,col="red")

# Look for tweets that were created in a certain timeframe
#AA_hist <- search_fullarchive(q = "#AmericanAirlines", n = 10000,env_name="FirstT",fromDate="201901010000",toDate="201912310000",token = twitter_token)

# look for a maximum of 1000 users
AA_users <- search_users(q = "americanair", n = 100)

library(ggplot2)

AA_users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(5)%>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")



# the actual tweet data is also retrieved:
AA_tweet <- tweets_data(AA_users)

# Look for 1000 followers of American Airlines
followers <- get_followers("AmericanAir", n = 1000)

# look up more information on these users (eg first 10) using sapply
followers_info <- lapply(followers[1:1000,],lookup_users)$user_id

# load some packages that we will use
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr','twitteR','syuzhet')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# Preprocessing with tidytext

#  Remove punctuation and numbers with regular expressions
tweets_clean <- mutate(Tweets_0302, text_clean = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

#  Tokenization (+ going to lowercase)
tweets_tokenized <- tweets_clean %>% unnest_tokens(output = "word", # how should the new column be named?
                                                  input = text_clean, # where can we find the text? 
                                                  token = "words", # which tokenization scheme should we follow?
                                                  drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase

#  Remove some other elements such as # and @ signs if they might occur
tweets_tokenized <- filter(tweets_tokenized, substr(word, 1, 1) != '#', 
                         substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags# Spelling correction 


# Remove stopwords

tweets_tokenized <-
  tweets_tokenized %>%
  anti_join(get_stopwords()) # note that we continue with the 'uncorrected' words here
# this strongly reduces the number of words


# Create the document-term matrix

# first, we need to get the number of times a word occurred in each document (or status, tweet)

tweets_tokenized <- 
  tweets_tokenized %>% 
  count(user_id,word) %>%
  filter(word != "airline",
         word != "americanairlines",
         word != "american",
         word != "americanair",
         word != "aa",
         word != "airlines")
head(tweets_tokenized)

# We perform weighting (e.g., tfidf) using the bind_tf_idf(word,id,n) function
# however, we will integreate this directly when making the document term matrix:

AA_DTM <- tweets_tokenized %>% 
  cast_dtm(user_id,word,n,weighting = tm::weightTfIdf)

# Reduce sparseness by removing the most sparse terms:
AA_DTM_Dense <- removeSparseTerms(AA_DTM,0.8)


# inspect our text

# We can look at associations/correlations between words (this is with the dtm):
american_assoc <- findAssocs(AA_DTM, terms = "american", corlimit = 0.1)

# Investigate the most frequent terms

tweets_freq <- tweets_tokenized %>%
  group_by(word) %>% # for this, we need to have the sum over all documents
  summarize(freq = n()) %>%
  arrange(-freq) # arrange = order; from most frequent term to lowest frequent
head(tweets_freq)


# Build a wordcloud in order to give this insight visually

# Load the package wordcloud
if (!require("wordcloud")) {
  install.packages("wordcloud",repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require("wordcloud")
}
# Word cloud based on the original text

# Use the termFreq of the tm package
# This also uses a tokenizer inside
tf <- termFreq(Tweets_0302$text)
wordcloud(names(tf),tf,
          max.words=40,
          scale=c(3,1))

# 2. Word cloud based on the tibble and all text pre-processing

#create word cloud
wordcloud(tweets_freq$word, tweets_freq$freq,
          max.words=50,
          scale=c(3,1))

library(ggplot2)

#Twitter Status
Tweets_0302 %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #AmericanAirlines Twitter statuses",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Frequency of Twitter statuses
tmls <- get_timelines("AmericanAir", n = 3200)

tmls %>%
  dplyr::filter(created_at > "2017-10-29") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by American Airlines",
    subtitle = "Twitter status (tweet) counts aggregated by day",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )



#Most used hashtags along with #americanairlines
twet <- mutate(Tweets_0302, hashtags = gsub(x = hashtags, pattern = '^c|"|[[:punct:]]|[()]', replacement = ""))

twet <- twet[,c("user_id","created_at","hashtags")]

twet_tokenized <- twet %>% unnest_tokens(output = "word", # how should the new column be named?
                                         input = hashtags, # where can we find the text? 
                                         token = "words", # which tokenization scheme should we follow?
                                         drop=FALSE,to_lower=TRUE)

if(!require("devtools")) install.packages("devtools"); library("devtools")
devtools::install_github("lchiffon/wordcloud2")
if(!require("wordcloud2")) install.packages("wordcloud2"); library("wordcloud2")

tf <- termFreq(twet_tokenized$word)
w <- data.frame(names(tf),tf)
colnames(w) <- c('word','freq')
figPath = system.file("examples/t.png",package = "wordcloud2")


wordcloud2(w,figPath = figPath,
           size = 2, color = "skyblue")
#refresh the viewer if the word cloud is not displayed

#We will first try to get the emotion score for each of the 
#tweets. 'Syuzhet' breaks the emotion into 10 different emotions - anger,
#anticipation, disgust, fear, joy, sadness, surprise,
#trust, negative and positive.
require(syuzhet)

head(tweets_clean$text_clean)

tweets_clean.df <- as.vector(tweets_clean$text_clean)

emotion.df <- get_nrc_sentiment(tweets_clean.df)

emotion.df2 <- cbind(tweets_clean$text_clean, emotion.df) 

head(emotion.df2)



sent.value <- syuzhet::get_sentiment(tweets_clean.df)

positive_tweets <- tweets_clean.df[sent.value>0]
negative_tweets <- tweets_clean.df[sent.value<0]
neutral_tweets <- tweets_clean.df[sent.value==0]



# Classify as Positive, Negative or Neutral tweets

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

category_senti2 <- cbind(tweets_clean$text_clean ,category_senti,sent.value) 

category_senti2 <- as_data_frame(category_senti2)

cat_senti3 <- cbind(category_senti2 ,tweets_clean$retweet_favorite_count)
cat_senti3 <- cbind(cat_senti3 ,tweets_clean$user_id)
cat_senti3 <- cbind(cat_senti3 ,tweets_clean$created_at)
cat_senti3 <- cbind(cat_senti3, tweets_clean$retweet_retweet_count)
# get column names
colnames(cat_senti3)

# Rename column
names(cat_senti3)[names(cat_senti3) == "V1"] <- "Text"
names(cat_senti3)[names(cat_senti3) == "sent.value"] <- "Sentiment_Value"
names(cat_senti3)[names(cat_senti3) == "tweets_clean$retweet_favorite_count"] <- "Likes"
names(cat_senti3)[names(cat_senti3) == "tweets_clean$user_id"] <- "User_ID"
names(cat_senti3)[names(cat_senti3) == "tweets_clean$created_at"] <- "Creation_Date"
names(cat_senti3)[names(cat_senti3) == "category_senti"] <- "Sentiment_Cat"
names(cat_senti3)[names(cat_senti3) == "tweets_clean$retweet_retweet_count"] <- "Comments"

cat_senti3$Sentiment_Value =as.numeric(cat_senti3$Sentiment_Value)
# Chanhe NA to 0
cat_senti3[is.na(cat_senti3)] <- 0


cat_senti3 %>%
  group_by(Sentiment_Cat)%>%
  hist(Sentiment_Value)


class(cat_senti3$Sentiment_Value)

# Installer
install.packages("wesanderson")
# Charger
library(wesanderson)

category_senti2 %>%
  count(category_senti, sort = TRUE) %>%
  mutate(category_senti = reorder(category_senti, n)) %>%
  ggplot(aes(x = category_senti, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = "Sentiment",
       title = "Sentiment Repartition over 3000 Tweets ")



sentiment = cat_senti3 %>% select(c("Text", "Sentiment_Cat"))
table = merge(x = Tweets_0302, y = sentiment, by.x = "text", by.y = "Text", all.x = TRUE)
saveRDS(table, file = ".\\table.rds")
