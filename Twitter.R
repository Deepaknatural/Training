

#  Install Requried Packages
install.packages("SnowballC")
install.packages("tm")
install.packages("twitteR")
install.packages("syuzhet")
install.packages("stringr")


# Load Requried Packages
library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")
library(stringr)



# Authonitical keys
consumer_key <- 'RmR2HvFe8gooN0tDOXj8TVE1t'
consumer_secret <- "4ldR7A0DbBJjmYIGNOUIj822145bZ2SO58CYkHjHmNR4AA3Du2"
access_token <- '1049520412634374144-zFB2pQpf8aAfwKNz3MvUqFuJisDHk5'
access_secret <- 'yLFju6a9g2yQ80qOVlkGQFXOvKWRwyduLEwv4PPwX23GG'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Twitter handle @realDonaldTrump
tweets <- userTimeline("realDonaldTrump", n=200)

n.tweet <- length(tweets)

tweets.df <- twListToDF(tweets) 

#Backup of tweets in a file
write.csv(tweets.df, file = "C:\\Teradata\\twitter\\MyData.csv")

#Reading again from tweets
tweets.df = read.csv("C:\\Teradata\\twitter\\MyData.csv")

head(tweets.df)

 


#Cleaning the tweets
#The field 'text' contains the tweet part, hashtags, and URLs. We need to remove hashtags and URLs from the text field
usableText=str_replace_all(tweets.df$text,"[^[:graph:]]", " ") 

tweets.df2 <- gsub("http.*","",usableText)

tweets.df2 <- gsub("https.*","",tweets.df2)

tweets.df2 <- gsub("#.*","",tweets.df2)

tweets.df2 <- gsub("@.*","",tweets.df2)

head(tweets.df2)



#Emotion score for each of the tweets.
word.df <- as.vector(tweets.df2)


#breaks the emotion into 10 different emotions - 
#anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative and positive.

emotion.df <- get_nrc_sentiment(word.df)

#Binding the emotion quotient with each tweet
emotion.df2 <- cbind(tweets.df2, emotion.df) 
head(emotion.df2)

#Getting sentiment score of each tweet
sent.value <- get_sentiment(word.df)

#Most Positive Sentence
most.positive <- word.df[sent.value == max(sent.value)]

most.positive

#Most Negative Sentence
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

#Sentiment score of each sentence
sent.value


#Categorising the sentiments as per their score
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)


#Binding the Result to dataset
category_senti2 <- cbind(tweets.df,category_senti,sent.value) 
head(category_senti2)


#Frequency distribution of sentiments of tweets of Donald Trump
table(category_senti)
