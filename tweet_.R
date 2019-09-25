#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
install.packages("tidytext")
library("dplyr")
library(tidytext)
library("twitteR")
install.packages("ROAuth")
library("ROAuth")
library("ggplot2")
cred <- OAuthFactory$new(consumerKey='AYjH5kAGgjFKRoJBIot7oowQC', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("AYjH5kAGgjFKRoJBIot7oowQC", # Consumer Key (API Key)
                    "Bb6u2ttLm8sGBLse4khJ1UziRuSnF3EsP6mNzX0aAsC4gjIxdm", #Consumer Secret (API Secret)
                    "1151714032295931904-fkksm7GqiZktRjLrLKsxYuCRhrxpEu",  # Access Token
                    "iZJiJsqPeBMZGu7UdkoPF7Awt5uf27qzwMmE6nHixe1CX")  #Access Token Secret

#registerTwitterOAuth(cred)
##Use  rtweet library to download 1000 tweets that the company posted. Save these tweets as "tweets.company".
tweets_company <- searchTwitter('Old School Business', n=1000, lang="en")
#Use  rtweet library to download 1000 tweets about the company you selected. Save these tweets as "tweets.public".
tweets_public <- searchTwitter('@user', n=1000, lang="en")


tweets.company = twitteR::twListToDF(tweets_company)
dim(tweets.company)
View(tweets.company)

tweets.public = twitteR::twListToDF(tweets_public)
dim(tweets.public)
View(tweets.public)
#table(tweets.company$statusSource)

###########company tweet cleaning
tweets.company$stripped_text <- gsub('<a href="//twitter.com/download/',"",  tweets.company$statusSource)
tweets.company$stripped_text <- gsub('<a href="//twitter.com/download/',"", tweets.company$stripped_text)


tweets.company_clean<- tweets.company %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

####company tweet most frequency word
tweets.company_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

############public tweet cleaning
tweets.public$stripped_text <- gsub('<a href="//twitter.com/download/',"",  tweets.public$statusSource)
tweets.public$stripped_text <- gsub('<a href="//twitter.com/download/',"", tweets.public$stripped_text)


tweets.public_clean<- tweets.public %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

####public tweet most frequency word
tweets.public_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

####################
##TFIDF
twcomp=tweets.company$text[1:67]
twpublic=tweets.public$text
tweets<-data.frame(cbind(twpublic,twcomp))



library(tm)
corpus <- iconv(tweets$twcomp, to = "utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

############
tdm <- DocumentTermMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]
w <- rowSums(tdm)

##############################
###Compute the most appropriate number of clusters using the elbow method for the combined tweets by using cosine distance.
distmatrix<-dist(scale(tdm),method="euclidean")

kmeans5<- kmeans(distmatrix, 5)
class(kmeans5)


kw_with_cluster <- data.frame(cbind(tweets$twcomp,kmeans5$cluster))
names(kw_with_cluster)[2] <- c("kmeans5")
head(kw_with_cluster)
dim(kw_with_cluster)
kw_with_cluster$num=1
no_clusters_vs_cost <- data.frame()

for(i in 1:20){
  #Run kmeans for each level of i, allowing up to 20 iterations for convergence
  kmeans<- kmeans(x=tdm, centers=i, iter.max=20)
  
  #Combine cluster number and costfunction together, write to df
  no_clusters_vs_cost<- rbind(no_clusters_vs_cost, cbind(i, kmeans$tot.withinss))
  
}
names(no_clusters_vs_cost) <- c("cluster", "cost")

ggplot(data=no_clusters_vs_cost, aes(x=cluster, y=cost)) + 
  theme_bw(base_family="Arial") + 
  geom_line(colour = "blue") +
  theme(text = element_text(size=10)) +
  ggtitle("Reduction In Cost For Values of 'k'") +
  xlab("Clusters") + 
  ylab("Within-Cluster Sum of Squares")

kmeans5<- kmeans(distmatrix, 4)
class(kmeans5)
kw_with_cluster <- data.frame(cbind(tweets$twcomp,kmeans5$cluster))
names(kw_with_cluster)[2] <- c("kmeans5")
head(kw_with_cluster)
dim(kw_with_cluster)
kw_with_cluster$num=1

#Split into different clusters

cluster1<-kw_with_cluster[kw_with_cluster$kmeans5==1,]
cluster2<-kw_with_cluster[kw_with_cluster$kmeans5==2,]
cluster3<-kw_with_cluster[kw_with_cluster$kmeans5==3,]
cluster4<-kw_with_cluster[kw_with_cluster$kmeans5==4,]


c1<-aggregate(kw_with_cluster$num,by=list(kw_with_cluster$kmeans5),sum)


##dendrogram
tweets.h<-hclust(distmatrix,method="ward")
cut<-cutree(tweets.h, k = 5)
plot(tweets.h,cex=0.1,hang=-1,which.plots = 2,main="Word cluster Dendogram")

install.packages("ggdendro")
library(ggdendro)
# basic option
ggdendrogram(tweets.h,cex=0.9)
ggdendrogram(tweets.h, rotate = TRUE, size = 4, theme_dendro = FALSE, color = "tomato")
# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(tweets.h, k = 5, cex=0.5,boxes = FALSE, col.up = "grey50", col.down = c("green", 
                                                                                "blue", "black","red","yellow","orange","brown"))

##################################
library(wordcloud)
words=names(w)
words
d=data.frame(word=words,freq=w)
wordcloud(d$word,d$freq,random.order=FALSE,colors=brewer.pal(8,'Dark2'))
    
                 