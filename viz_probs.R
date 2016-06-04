setwd("/home/yz/Documentos/BigData/Project!!!")
tweetframe <- read.csv(file="tourists_residents_eng.csv", header=TRUE, sep=",")
# required pakacges

library(tm)
library(wordcloud)
library(stringr)
library(dplyr)

tweets <- tweetframe
table(tweets$tourist)

#Cleaning function
clean.text <- function(tweets)
{
  tweets$tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets$tweet)
  tweets$tweet = gsub("[[:punct:]]", "", tweets$tweet)
  tweets$tweet = gsub("@\\w+", "", tweets$tweet)
  tweets$tweet = gsub("http\\w+", "", tweets$tweet)
  tweets$tweet = gsub("https\\w+", "", tweets$tweet)
  tweets$tweet = gsub("[[:digit:]]+", "", tweets$tweet)
  tweets$tweet = gsub("[ \t]{2,}", "", tweets$tweet)
  tweets$tweet = gsub("^ ", "", tweets$tweet)
  tweets$tweet = gsub(" $", "", tweets$tweet)
  tweets$tweet = tolower(tweets$tweet)
  return(tweets)
}

#Applying cleaning function

tweets$tweet = clean.text(tweets)

#Dividing for different tourist and resident subframes
tourtweet <- subset(tweets, tweets$tourist=="TRUE",
                    select=tweet)

restweet <- subset(tweets, tweets$tourist=="FALSE",
                    select=tweet)

#Join tweets in a vector for each group
tour = paste(tourtweet$tweet, collapse=" ")
res = paste(restweet$tweet, collapse=" ")

# put everything in a single vector
all = c(tour, res)

# remove stop-words
all = removeWords(all,stopwords("english")) #"russian", "french"

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

# add column names
colnames(tdm) = c("Tourists", "Residents")

# comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("blue", "red"),
                 title.size=1.5, max.words=200)
# commonality cloud
commonality.cloud(tdm, random.order=FALSE, 
                  colors = brewer.pal(8, "Dark2"),
                  title.size=1.5)


