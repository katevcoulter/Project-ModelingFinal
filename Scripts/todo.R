tweets <- read.csv("Final Project/Data/tweets.csv")
View(tweets)
tweets <- tweets %>% select(text, created_at, retweet_count, favorite_count)
date <- tweets$created_at

test <- read.table(text = as.character(tweets$created_at), sep = " ")$V1
tweets$date <- test
tweets <- tweets %>% select(text, RT, fav, date)
tweetsinday <- table(tweets$date)


test <- read.table(text = as.character(tweets$date), sep = "-")
tweets$date2 <- as.Date(with(test, paste(V3, V1, V2, sep="-")), "%Y-%m-%d")
test <- read.table(text = as.character(ratings$date), sep = "/")
ratings$date <- as.Date(with(test, paste(V3, V1, V2, sep="-")), "%Y-%m-%d")

nov3 <- data.frame("date" = as.Date("2018-11-03"), "approve" = 42.15896, "disapprove" = 52.83494)
ratings <- rbind(ratings, nov3)

test <- merge(tweetcount, dates, by="date", all=TRUE)

stats <- summarize(group_by(tweets, date), RT_count=sum(RT), fav_count = sum(fav))
stats <- summarize(group_by(tweets, date), RT_count=sum(RT), fav_count = sum(fav), RT_avg=mean(RT), fav_avg=mean(fav))
trump <- merge(trump, stats, by="date", type="left", all=TRUE)




# how to define viral tweets
g <- ggplot(tweets, aes(x=RT)) + geom_density()
