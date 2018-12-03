


#########################################################################
# wordcloud(words,freq,scale=c(4,.5),min.freq=3,max.words=Inf,
#           random.order=TRUE, random.color=FALSE, rot.per=.1,
#           colors="black",ordered.colors=FALSE,use.r.layout=FALSE,
#           fixed.asp=TRUE, ...)

# Arguments
# words the words
# freq their frequencies
# scale A vector of length 2 indicating the range of the size of the words.
# min.freq words with frequency below min.freq will not be plotted
# max.words Maximum number of words to be plotted. least frequent terms dropped
# random.order plot words in random order. If false, they will be plotted in decreasing frequency
# random.color choose colors randomly from the colors. If false, the color is chosen based on
# the frequency
# rot.per proportion words with 90 degree rotation
# colors color words from least to most frequent
# ordered.colors if true, then colors are assigned to words in order
# use.r.layout if false, then c++ code is used for collision detection, otherwise R is used
# fixed.asp if TRUE, the aspect ratio is fixed. Variable aspect ratio only supported if rot.per==0
# ... Additional parameters to be passed to text (and strheight,strwidth).






q1 <- Corpus(VectorSource(tweets.df2))
crude <- tm_map(q1, removePunctuation)
crude <- tm_map(crude, function(x)removeWords(x,stopwords()))

wordcloud(crude)


tdm <- TermDocumentMatrix(crude)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word,d$freq)


#A bigger cloud with a minimum frequency of 2
wordcloud(d$word,d$freq,c(8,.3),2)
#Now lets try it with frequent words plotted first
wordcloud(d$word,d$freq,c(8,.5),2,,FALSE,.1)

library(RColorBrewer)



pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(d$word,d$freq,c(8,.3),2,,FALSE,,.15,pal)
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]
wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,,.15,pal)
#random colors
wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,TRUE,.15,pal)

