#rm(list=ls(all=TRUE))

approval <- read.csv("/Users/kathryncoulter/Desktop/Final Project/Data/approval_topline.csv")
View(approval)
3*365
uniquedates <- unique(approval$modeldate)
uniquedates
approval.raw <- approval
approval %>% slice(61:)
approval %>% slice(61:2040)
library(dplyr)
approval %>% slice(61:)
approval %>% slice(61:2040)
approval <- approval %>% slice(61:2040)
View(approval.raw)
approval <- approval.raw %>% slice(57:2040)
dim(approval)
661*3
approval <- approval.raw %>% slice(2:1983)
dim(approval)
approval <- approval.raw %>% slice(58:2040)
dim(approval)
661*3
43.95796 + 41.37979
85.33775/2
approval <- approval %>% filter(approval, subgroup =="All polls")
dim(approval)
uniquedays <- unique(approval$modeldate)
dim(uniquedays)
1983/3
table(approval$modeldate)
dates <- table(approval$modeldate)
table
dates
summary(dates)
summary(approval$modeldate)
approval <- approval %>% filter(approval[1:1957])
approval <- approval %>% filter(approval[,1:1957])
approval <- approval %>% filter(approval[1:1957,])
approval <- approval %>% slice(approval[1:1957])
approval <- approval %>% slice(1:1957)
approval <- approval %>% slice(1:1956)
approval %>% group_by(modeldate) %>% summarize(count=n())
approval %>% group_by(modeldate) %>% summarize(count=n())
1956/3
approval <- filter(approval, subgroup == "All polls")
count(unique(approval$subgroup))
table(approval$subgroup)
approval$date <- approval$modeldate
approval <- approval %>% select(date, approve_estimate, disapprove_estimate)
ratings <- approval
library(data.table)
setnames(ratings, old=c("date", "approve_estimate", "disapprove_estimate"), new=c("date", "approve", "disapprove"))
View(ratings)
#rm(approval)
#rm(approval.raw)
#rm(pollist)
#rm(dates)
#rm(uniquedates)
#rm(uniquedays)
summary(ratings)
tweets <- read.csv("/Users/kathryncoulter/Desktop/Final Project/Data/tweets.csv")

tweets <- tweets %>% select(text, created_at, retweet_count, favorite_count)
date <- tweets$created_at
tweets <- tweets %>% select(text, created_at, retweet_count, favorite_count)
tweets <- read.csv("Final Project/Data/tweets.csv")
date <- tweets$created_at
table(tweets)
summary(tweets)
read.table(text = as.character(tweets$created_at), sep = " ")$V1
test <- read.table(text = as.character(tweets$created_at), sep = " ")$V1
tweets <- read.csv("Final Project/Data/tweets.csv")
View(tweets)
tweets <- select(text, created_at, retweet_count, favorite_count)
tweets <- tweets %>% select(text, created_at, retweet_count, favorite_count)
date <- tweets$date
date <- tweets$created_at
date <- str_split(date, " ", simplify = TRUE)
date <- date[,c(3,2,6,4,1,5)]
date <- date[, -c(5:6)]
date <- as.data.frame(date)
date <- unite(date, "day", sep=" ", c(1:3))
date$day <- fo#rmat(as.Date(date$day, "%d %b %Y"), "%Y-%m-%d")
date <- unite(date, "date", sep=" ", c(1:2))
library(tidyverse)
date <- str_split(date, " ", simplify = TRUE)
date <- date[,c(3,2,6,4,1,5)]
date <- date[, -c(5:6)]
date <- as.data.frame(date)
date <- unite(date, "day", sep=" ", c(1:3))
date$day <- fo#rmat(as.Date(date$day, "%d %b %Y"), "%Y-%m-%d")
date <- unite(date, "date", sep=" ", c(1:2))
View(date)
tweets <- read.csv("Final Project/Data/tweets.csv")




View(tweets)
tweets <- tweets %>% select(text, created_at, retweet_count, favorite_count)
date <- tweets$created_at
tweets <- tweets %>% select(text, created_at, retweet_count, favorite_count)
tweets <- read.csv("Final Project/Data/tweets.csv")
date <- tweets$created_at
table(tweets)
summary(tweets)
read.table(text = as.character(tweets$created_at), sep = " ")$V1
test <- read.table(text = as.character(tweets$created_at), sep = " ")$V1
tail(test)
head(test)
tweets$date <- test
approval <- read.csv("Final Project/Data/approval_topline.csv")
getwd()
approval <- read.csv("Final Project/Data/approval_topline.csv")
getwd()
approval <- read.csv("/Users/kathryncoulter/Desktop/Final Project/Data/approval_topline.csv")
approval <- read.csv("/Users/kathryncoulter/Desktop/Final Project/Data/approval_topline.csv")
View(approval)
3*365
uniquedates <- unique(approval$modeldate)
uniquedates





approval.raw <- approval
approval %>% slice(61:)
approval %>% slice(61:2040)
library(dplyr)
approval %>% slice(61:)
approval %>% slice(61:2040)
approval <- approval %>% slice(61:2040)
View(approval.raw)
approval <- approval.raw %>% slice(57:2040)
dim(approval)
661*3
approval <- approval.raw %>% slice(2:1983)
dim(approval)
approval <- approval.raw %>% slice(58:2040)
dim(approval)
661*3
43.95796 + 41.37979
85.33775/2
approval <- approval %>% filter(approval, subgroup =="All polls")
dim(approval)
uniquedays <- unique(approval$modeldate)
dim(uniquedays)
1983/3
table(approval$modeldate)
dates <- table(approval$modeldate)
table
dates
summary(dates)
summary(approval$modeldate)
test <- read.table(text = as.character(tweets$created_at), sep = " ")$V1
tail(test)
head(test)
tweets$date <- test
test <- as.Date(test)
test <- as.character(test)
test <- as.Date(test)
test <- as.Date(test,"%m-%b-%Y")
tweets$date <- test
test
test <- read.table(text = as.character(tweets$created_at), sep = " ")$V1
test <- as.Date(test,"%m-%b-%Y")
test
test <- read.table(text = as.character(tweets$created_at), sep = " ")$V1
test <- read.table(text = as.character(tweets$created_at), sep = " ")$V1
test
test <- as.Date(test,"%m-%b-%Y")
test
test <- as.Date(test,"%m-%b-%Y ")
test
test <- read.table(text = as.character(tweets$created_at), sep = " ")$V1
tweets$date <- test
setnames(tweets, old=c("retweet_count", "favorite_count"), new=c("RT", "fav"))
tweets
setnames(tweets, old=c("retweet_count", "favorite_count"), new=c("RT", "fav"))
summary(tweets)
tweets <- tweets %>% select(text, RT, fav, date)
tweetsinday <- table(tweets$date)
test <- read.table(text = as.character(tweets$date), sep = "-")
test
year <- test$V3
month <- test$V1
day <- test$V2
class(year)
class(test)
test
tweets$date2 <- as.Date(with(test, paste(V3, V1, V2, sep="-")), "%Y-%m-%d")
tweets <- subset(tweets, select = -4)
names(tweets)[4] <- "date"
test <- read.table(text = as.character(ratings$date), sep = "/")
ratings$date <- as.Date(with(test, paste(V3, V1, V2, sep="-")), "%Y-%m-%d")
#rm(test)
#rm(date)
#rm(day)
#rm(month)
#rm(tweetsinaday)
#rm(tweetsinday)
#rm(year)
dates <- data.frame(date = seq(as.POSIXct("2017-02-01"), as.POSIXct("2018-11-15"), by="day"))
tail(dates)
View(dates)
dates <- data.frame(ratings$date)
dates$date <- dates$ratings.date



test643 <- table(dates)
nov3 <- data.frame("date" = as.Date("2018-11-03"),count=20, maxresponse = 200200,  "approve" = 42.15896, "disapprove" = 52.83494)
nov3
ratings <- rbind(ratings, nov3)
#rm(nov3)
#rm(test2)
test
trump <- merge(ratings, test, by="date", all=TRUE)
trump[is.na(trump)] <- 0
summary(trump)
RT <- tweets %>% group_by(date) %>% summarize(funs(sum()))
View(RT)
RT <- tweets %>% group_by(date) %>% summarize(funs(sum))
View(ratings)
View(RT)
RT <- tweets %>% group_by(date) %>% summarise_each(funs(sum))
stats <- summarize(group_by(tweets, date), RT_count=sum(RT), fav_count = sum(fav))
sum(tweets$RT[1:9])
View(stats)
stats <- summarize(group_by(tweets, date), RT_sum=sum(RT), fav_sum = sum(fav), RT_avg=mean(RT), fav_avg=mean(fav))
#rm(RT)
#rm(test)
#rm(tweetcount)
#rm(dates)
test <- merge(trump, stats, by="date", type="left", all=TRUE)
trump <- test
#rm(test)
#rm(stats)
#rm(test643)
View(trump)
View(tweets)
summary(tweets)
RTs <- ggplot(tweets, aes(x = RT))
RTs
RTs <- ggplot(tweets, aes(x = RT)) + geom_density()
RTs
g <- ggplot(tweets, aes(x=RT)) + geom_density()
g + geom_histogram()
tweets$response <- tweets$RT + tweets$fav

combine$viral <- cut(combine$maxresponse, c(0,232291, 272299, 402289, 974628), labels=c("NA", "90", "95", "99"))

tweets.df2 <- tweets$text

tweets.df2 <- gsub("http.*","",tweets$text)
tweets.df2 <- gsub(" .*t.co.* ","",tweets.df2)
tweets.df2 <- gsub("https.* ","",tweets.df2)

clean_tweet = gsub("&amp", " ", tweets.df2)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", clean_tweet)
clean_tweet = gsub("@\\w+", " ", clean_tweet)
clean_tweet = gsub("[[:punct:]]", " ", clean_tweet)
clean_tweet = gsub("[[:digit:]]", " ", clean_tweet)
clean_tweet = gsub("http\\w* ", "", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", " ", clean_tweet) 
clean_tweet = gsub("https\\w* ", "", clean_tweet)

head(clean_tweet)
data("stop_words")
tidy_trump_tweets<-tidy_trump_tweets %>%
  anti_join(stop_words)




tweets.df2 <- gsub("http.*","",tweets$text)
tweets.df2 <- gsub(" .*t.co.* ","",tweets.df2)
tweets.df2 <- gsub("https.* ","",tweets.df2)

tweets.df2 <- gsub("#.* ","",tweets.df2)

tweets.df2 <- gsub("@.* ","",tweets.df2)





process_sentiment <- function (rawtext, mymethod) {
  chunkedtext <- data_frame(x = rawtext) %>% 
    group_by(linenumber = ceiling(row_number() / 10)) %>% 
    summarize(text = str_c(x, collapse = " "))
  mySentiment <- data.frame(cbind(linenumber = chunkedtext$linenumber, 
                                  sentiment = get_sentiment(chunkedtext$text, method = mymethod)))
}



quantile(tweets$response, c(.8, .85, .9, .95, .97, .99))
quantile(tweets$response, c(.8, .85, .9, .95, .98, .99))
sum(tweets$response >= 134822)
sum(tweets$response >= 146407)
sum(tweets$response >= 160636)
sum(tweets$response >= 186903)
sum(tweets$response >= 205708)
tweets <- tweets %>% mutate(viral80 = ifelse(response == 134822), TRUE)
tweets$viral80 <- NA
tweets$viral90 <- NA
tweets$viral97 <- NA
tweets <- mutate(tweets, viral80 = ifelse(response > 134821), 1, 0)
tweets <- tweets %>% mutate(viral80 = ifelse(response > 134821, '1', '0'))
tweets <- tweets %>% mutate(viral90 = ifelse(response > 160635, '1', '0'))
tweets <- tweets %>% mutate(viral97 = ifelse(response > 205708, '1', '0'))
tweets %>% group_by(date) %>% summarise(viral80)
library(tidytext)
library(rtweet)
library(proustr)
install.packages("proustr")
library(proustr)
install.packages(syuzhet)
tweets$sentiment <- get_sentiments(tweets$text)
tweets2 <- gsub("http.*", "", tweets$text)
tweets$text2 <- gsub("http.*", "", tweets$text)
tweets$text2 <- gsub("https.*", "", tweets$text)
tweets$text2 <- gsub("#.*", "", tweets$text)
tweets$text2 <- gsub("@.*", "", tweets$text)
sa <- as.vector(tweets$text2)
sa.df <- get_sentiments(sa)
library(twitteR)
#  Install Requried Packages
installed.packages("SnowballC")
installed.packages("tm")
installed.packages("twitteR")
installed.packages("syuzhet")
install.packages("syuzhet")
library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")
tweets$text2 <- NA
sentiment <- get_nrc_sentiment(tweets$text)
sentiment <- get_nrc_sentiment(as.vector(tweets$text))
tweets$text2 <- plain_tweets(tweets$text)
tweets$text2.1 <- plain_tweets(tweets$text, tokenize=TRUE)
sa <- plain_tweets(tweets$text2, tokenize=TRUE)
sa
sa <- plain_tweets(tweets$text2, tokenize=TRUE)
sa <- table(unlist(stopwords))
sa <- plain_tweets(tweets$text2)
install.packages("text2vec")
library("text2vec")
sa
sa <- word_tokenizer(sa)
sa
sa <- table(unlist(stopwords()))
sa <- table(unlist(stopwords)
)
sa <- syuzhet::get_nrc_sentiment(sa)
sa <- tolower(sa)
tweets$text2 <- plain_tweets(tweets$text)
clean <- as.data.frame(tweets$text2)
nrow(clean)
nrc <- get_sentiments(clean)
nrc <- get_sentiments("nrc")
View(clean)
clean$ID <- seq[1:4449]
clean$ID <- seq(1:4449)
clean <- clean %>% unnest_tokens(word, tweets$text2)
clean <- tweets$text2
clean.table <- tibble(tweetID = seq_along(clean), text=clean)
View(clean.table)
clean.table <- clean.table %>% unnest_tokens(word, text)
clean.table <- clean.table %>% anti_join(stop_words)
clean.table %>% count(word, sort = TRUE)
clean.table <- clean.table %>% anti_join("t.co")
stop_words
stop_words <- merge(stop_words, c("t.co", "SMART"))
stop_words
stop_words <- mutate(stop_words, -y)
class(stop_words)
stop_words <- stop_words[, -3]
View(stop_words)
stop_words <- stop_words[, -4]
View(stop_words)
stop_words <- stop_words[, -3]
stop_words <- rbind(stop_words, c("t.co", "SMART"))
dim(stop_words)
stop_words <- rbind(stop_words, c("t.co", "onix"))
stop_words <- rbind(stop_words, c("t.co", "snowball"))
clean.table <- clean.table %>% anti_join(stop_words)
clean.table %>% count(word, sort = TRUE)
clean.table <- clean.table %>% right_join(get_sentiments("nrc")) %>% filter(!is.na(sentiment)) %>% count(sentiment, sort=TRUE)
clean.table
sentimentcount <- clean.table
clean.table <- clean.table %>% anti_join(stop_words)
tweets2 <- tweets$text
word.df <- as.vector(tweets2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(emotion.df2, emotion.df)
emotion.df2 <- cbind(tweets2, emotion.df)
head(emotion.df2)
View(emotion.df2)
tweets <- merge(tweets, emotion.df2, by="tweets2")
tweets <- tweets %>% dplyr::rename(text2=tweets2)
tweets$tweets2 <- tweets$text2
tweets <- merge(tweets, emotion.df2, by="tweets2")
sa.value
sa.value <- get_sentiment(word.df)
sa.value
sa.value <- data.frame(sa.value)
tweets <- merge(tweets, sa.value)











vars <- trump %>% select(date, RT_sum, fav_sum, count, )
