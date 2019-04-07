install.packages("twitteR")
install.packages("ROAuth")
install.packages("syuzhet")
# install.packages("stringi")
install.packages("tm")
install.packages("SnowballC")
#library(NLP)
library(twitteR)
library(syuzhet)
library(tm)
library(SnowballC)
#library(stringi)
#library(topicmodels)
library(syuzhet)
library(twitteR)
library(ROAuth)
library(ggplot2)



consumer_key <- ''
consumer_secret <- ""
access_token <- ''
access_secret <- ''


setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#Extracting tweets using a particular hashtag:

tweets_g <- searchTwitter("#google", n=1000,lang = "en")
tweets_a <- searchTwitter("#amazon", n=1000,lang = "en")
tweets_f <- searchTwitter("#facebook", n=1000,lang = "en")



#Convert this extracted data to a dataframe which makes it more readable and easier to work with.

amazon_tweets <- twListToDF(tweets_a)
google_tweets <- twListToDF(tweets_g)
facebook_tweets <- twListToDF(tweets_f)

head(facebook_tweets)



#Below is a code to pre-process the data and remove tabs, blank spaces, links etc


google_text<- google_tweets$text
amazon_text<- amazon_tweets$text
facebook_text<- facebook_tweets$text

#convert all text to lower case
google_text<- tolower(google_text)
amazon_text<- tolower(amazon_text)
facebook_text<- tolower(facebook_text)

# Replace blank space ("rt")
google_text <- gsub("rt", "", google_text)
amazon_text <- gsub("rt", "", amazon_text)
facebook_text <- gsub("rt", "", facebook_text)

# Replace @UserName
google_text <- gsub("@\\w+", "", google_text)
amazon_text <- gsub("@\\w+", "", amazon_text)
facebook_text <- gsub("@\\w+", "", facebook_text)


# Remove punctuation
google_text <- gsub("[[:punct:]]", "", google_text)
amazon_text <- gsub("[[:punct:]]", "", amazon_text)
facebook_text <- gsub("[[:punct:]]", "", facebook_text)


# Remove links
google_text <- gsub("http\\w+", "", google_text)
amazon_text <- gsub("http\\w+", "", amazon_text)
facebook_text <- gsub("http\\w+", "", facebook_text)

# Remove tabs
google_text <- gsub("[ |\t]{2,}", "", google_text)
amazon_text <- gsub("[ |\t]{2,}", "", amazon_text)
facebook_text <- gsub("[ |\t]{2,}", "", facebook_text)


# Remove blank spaces at the beginning
google_text <- gsub("^ ", "", google_text)
amazon_text <- gsub("^ ", "", amazon_text)
facebook_text <- gsub("^ ", "", facebook_text)

# Remove blank spaces at the end
google_text <- gsub(" $", "", google_text)
amazon_text <- gsub(" $", "", amazon_text)
facebook_text <- gsub(" $", "", facebook_text)



google_tweets.text.corpus <- Corpus(VectorSource(google_text))
google_tweets.text.corpus <- tm_map(google_tweets.text.corpus, function(x)removeWords(x,stopwords()))

amazon_tweets.text.corpus <- Corpus(VectorSource(amazon_text))
amazon_tweets.text.corpus <- tm_map(amazon_tweets.text.corpus, function(x)removeWords(x,stopwords()))

facebook_tweets.text.corpus <- Corpus(VectorSource(facebook_text))
facebook_tweets.text.corpus <- tm_map(facebook_tweets.text.corpus, function(x)removeWords(x,stopwords()))


install.packages("wordcloud")
library(wordcloud)
#generate wordcloud
wordcloud(google_tweets.text.corpus,min.freq = 10,colors=brewer.pal(5, "Dark2"),random.color = TRUE,max.words = 50)
wordcloud(amazon_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(facebook_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)




#Sentiment Analysis
#getting emotions using in-built function
mysentiment_google<-get_nrc_sentiment((google_text))
print(mysentiment_google)
mysentiment_amazon<-get_nrc_sentiment((amazon_text))
mysentiment_facebook<-get_nrc_sentiment((facebook_text))



#calculationg total score for each sentiment
Sentimentscores_google<-data.frame(colSums(mysentiment_google[,]))
Sentimentscores_amazon<-data.frame(colSums(mysentiment_amazon[,]))
Sentimentscores_facebook<-data.frame(colSums(mysentiment_facebook[,]))

head(Sentimentscores_facebook)

names(Sentimentscores_google)<-"Score"
Sentimentscores_google<-cbind("sentiment"=rownames(Sentimentscores_google),Sentimentscores_google)
print(Sentimentscores_google)
rownames(Sentimentscores_google)<-NULL

names(Sentimentscores_amazon)<-"Score"
Sentimentscores_amazon<-cbind("sentiment"=rownames(Sentimentscores_amazon),Sentimentscores_amazon)
rownames(Sentimentscores_amazon)<-NULL

names(Sentimentscores_facebook)<-"Score"
Sentimentscores_facebook<-cbind("sentiment"=rownames(Sentimentscores_facebook),Sentimentscores_facebook)
rownames(Sentimentscores_facebook)<-NULL


#*************************************************************************************

#plotting the sentiments with scores
ggplot(data=Sentimentscores_google,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech giant GOOGLE")


ggplot(data=Sentimentscores_amazon,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on ecomerce giant AMAZON")


ggplot(data=Sentimentscores_facebook,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Social Netwoking site FACEBOOK")




# Comparison word cloud for Google
all = c(
  paste(google_text[mysentiment_google$anger > 0], collapse=" "),
  paste(google_text[mysentiment_google$anticipation > 0], collapse=" "),
  paste(google_text[mysentiment_google$disgust > 0], collapse=" "),
  paste(google_text[mysentiment_google$fear > 0], collapse=" "),
  paste(google_text[mysentiment_google$joy > 0], collapse=" "),
  paste(google_text[mysentiment_google$sadness > 0], collapse=" "),
  paste(google_text[mysentiment_google$surprise > 0], collapse=" "),
  paste(google_text[mysentiment_google$trust > 0], collapse=" ")
)
all <- removeWords(all, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(all))
#
# create term-document matrix
tdm = TermDocumentMatrix(corpus)
#
# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]
#
# add column names
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)







# Comparison word cloud for Amazon
all = c(
  paste(amazon_text[mysentiment_amazon$anger > 0], collapse=" "),
  paste(amazon_text[mysentiment_amazon$anticipation > 0], collapse=" "),
  paste(amazon_text[mysentiment_amazon$disgust > 0], collapse=" "),
  paste(amazon_text[mysentiment_amazon$fear > 0], collapse=" "),
  paste(amazon_text[mysentiment_amazon$joy > 0], collapse=" "),
  paste(amazon_text[mysentiment_amazon$sadness > 0], collapse=" "),
  paste(amazon_text[mysentiment_amazon$surprise > 0], collapse=" "),
  paste(amazon_text[mysentiment_amazon$trust > 0], collapse=" ")
)
all <- removeWords(all, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(all))
#
# create term-document matrix
tdm = TermDocumentMatrix(corpus)
#
# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]
#
# add column names
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)






# Comparison word cloud Facebook
all = c(
  paste(facebook_text[mysentiment_facebook$anger > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$anticipation > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$disgust > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$fear > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$joy > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$sadness > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$surprise > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$trust > 0], collapse=" ")
)
all <- removeWords(all, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(all))
#
# create term-document matrix
tdm = TermDocumentMatrix(corpus)
#
# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]
#
# add column names
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)




# Comparison word cloud
all = c(
  paste(facebook_text[mysentiment_facebook$anger > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$anticipation > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$disgust > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$fear > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$joy > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$sadness > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$surprise > 0], collapse=" "),
  paste(facebook_text[mysentiment_facebook$trust > 0], collapse=" ")
)
all <- removeWords(all, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(all))
#
# create term-document matrix
tdm = TermDocumentMatrix(corpus)
#
# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]
#
# add column names
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

