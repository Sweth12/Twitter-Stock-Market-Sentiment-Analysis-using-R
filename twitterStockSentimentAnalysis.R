rm(list=ls()); cat("\014") # clear all
library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)
require(ROAuth)
library(tidyquant)
library(timetk)
library(tidyverse)
library("bitops")
library("RCurl")
library("rjson")
library(plotly)
library(quantmod)
library("twitteR")
library("googleVis")


# Use your own Twitter Developer API Key and Secret
t.api.key <- ""
t.api.secret <- ""
t.access.token <- ""
t.access.secret <- ""

# ---------  Save Credentials --------- 
setup_twitter_oauth(consumer_key = t.api.key, consumer_secret = t.api.secret, 
                    access_token = t.access.token, 
                    access_secret = t.access.secret)
save(list=(c("t.api.key","t.api.secret","t.access.token","t.access.secret")), file="twitter_credentials.RData")

# load twitter credentials

t.cred <- load("twitter_credentials.RData")
setup_twitter_oauth(t.api.key, t.api.secret, t.access.token, t.access.secret)

# Get top three gainers

# As tweets keep changing I have saved the tweets file(collected on 2019-12-05) and attached in the folder

# A. GET TWEETS OF GAINERS
twtPDCO <- searchTwitter('$PDCO', n = 100)
twtACAD <- searchTwitter('$ACAD', n = 100)
twtRH <- searchTwitter('$RH', n = 100)
#save(twtPDCO, file="twtPDCO")   
#save(twtACAD, file="twtACAD")
#save(twtRH, file="twtRH")
load(file = "twtPDCO")
load(file = "twtACAD")
load(file = "twtRH")

gainers = c(twtPDCO,twtACAD,twtRH)

# A. GET TWEETS OF LOSERS
twtSAGE <- searchTwitter('$SAGE', n = 100)
twtESTC <- searchTwitter('$ESTC', n = 100)
twtKOD <- searchTwitter('$KOD', n = 100)
#save(twtSAGE, file="twtSAGE")
#save(twtESTC, file="twtESTC")
#save(twtKOD, file="twtKOD")
load(file = "twtSAGE")
load(file = "twtESTC")
load(file = "twtKOD")

losers = c(twtSAGE,twtESTC,twtKOD)

#display tweets

display.tweet <- function (tweets) 
{
  cat("Screen name:", tweets$getScreenName(), "\nText:", tweets$getText(), "\n\n")
}

for (t in twtKOD)
{
  display.tweet(t)
}

# B. CREATE TWO DATA CORPUS FOR THE TWEETS
getCorpus <- function (tweets) {
  tweets.text <- lapply(tweets, function(t) {t$getText()})
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
  return (data.corpus)
}

gainers.corp <- getCorpus(gainers)
gainers.df<-data.frame(text=unlist(sapply(gainers.corp, `[`, "content")),stringsAsFactors=F)
writeCorpus(gainers.corp)

losers.corp = getCorpus(losers)
losers.df<-data.frame(text=unlist(sapply(losers.corp, `[`, "content")),stringsAsFactors=F)
writeCorpus(losers.corp)

# C. PRE-PROCESS DATA
removeURL <- function(x) {  
  gsub("(http[^ ]*)", "", x) 
}

removeNumberWords<-function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])([[:cntrl:]])*", "", x)
  iconv(x, "latin1", "ASCII", sub="")
}

getTransCorpus<-function (data.corpus) {
  data.corpus<-tm_map(data.corpus, content_transformer(removeURL))
  data.corpus<-tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus<-tm_map(data.corpus, content_transformer(tolower)) 
  english.stopwords<-stopwords("en")
  data.corpus<-tm_map(data.corpus,content_transformer(removeWords),english.stopwords)
  data.corpus<-tm_map(data.corpus, content_transformer(removeNumberWords))
  data.corpus<-tm_map(data.corpus,content_transformer(stemDocument))
  data.corpus<-tm_map(data.corpus,content_transformer(stripWhitespace))
  return (data.corpus)
}

tgainers.corp = getTransCorpus(gainers.corp)
tlosers.corp = getTransCorpus(losers.corp)


# D. CREATE TERM DOCUMENT MATRIX FOR EACH TWEET
tdmGainers <- TermDocumentMatrix(tgainers.corp, control = list(wordLengths = c(3,10)))
tdmLosers <- TermDocumentMatrix(tlosers.corp, control = list(wordLengths = c(3,10)))

#save(tdmGainers, file="tdmGainers")
#save(tdmLosers, file="tdmLosers")

#Converting TDM to a matrix
m1 <- as.matrix(tdmGainers)

m2 <- as.matrix(tdmLosers)

#Calculate the frequency of words 
wordFreq1 <- rowSums(m1)
wordFreq2 <- rowSums(m2)

#Sort the words by descending order of frequency
wordFreq1 <- sort(wordFreq1, decreasing=TRUE)
wordFreq2 <- sort(wordFreq2, decreasing=TRUE)

#Frequent terms sets
findFreqTerms(tdmGainers, lowfreq=10)
findFreqTerms(tdmLosers, lowfreq=10)

#Top 10 words in Gainers and Losers corpus
wordFreq1[1:10]
wordFreq2[1:10]

par(mfrow=c(1,2))
set.seed(137)
barplot(wordFreq1[1:10], col = 'blue', main='Top 10 words of Gainers stocks', las=2, ylim = c(0,100))
barplot(wordFreq2[1:10], col = 'blue', main='Top 10 words in Losers stocks', las=2)
par(mfrow=c(1,1))

#Word cloud
library(wordcloud)

palette <- brewer.pal(8,"Dark2")
palette
par(mfrow=c(1,2))
set.seed(137)
wordcloud(words=names(wordFreq1), 
          freq=wordFreq1, 
          min.freq=10, 
          random.order=F,
          colors=palette)

wordcloud(words=names(wordFreq2), 
          freq=wordFreq2, 
          min.freq=10, 
          random.order=F,
          colors=palette)
par(mfrow=c(1,1))

#Lexicons
# positive-words.txt and negative-words.txt attached in the folder
pos.words = scan('positive-words.txt', what='character', comment.char=';')

neg.words = scan('negative-words.txt', what='character', comment.char=';')

head(pos.words)

head(neg.words)

sentiment <- function(text, pos.words, neg.words)
{
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text) # \\d represents digit
  text <- tolower(text)
  
  #Split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  
  #Find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  
  #Find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  
  #Calculate the sentiment score
  score <- sum(pos.matches) - sum(neg.matches)
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}


save(list="gainers",
     file="tweetgainer.RData")

save(list="losers",
     file="tweetloser.RData")

load(file="tweetgainer.RData")
load(file="tweetloser.RData")


tweetgainer.texts <- lapply(gainers, function(t) {
  iconv(t$getText(), "latin1", "ASCII", sub="")})


tweetloser.texts <- lapply(losers, 
                           function(t) {
                             iconv(t$getText(), "latin1", "ASCII", sub="")})

sink(tempfile())

#Sapply works on a list or vector of data
scores1 <- sapply(tweetgainer.texts, sentiment, pos.words, neg.words)


scores2 <- sapply(tweetloser.texts, sentiment, pos.words, neg.words)


sink()


table(scores1)
table(scores2)
par(mfrow=c(1,2))

barplot(table(scores1), 
        xlab="Scores", ylab="Count",
        col="cyan", main = 'Sentiment scores of Gainers')

grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)

barplot(table(scores2), 
        xlab="Score", ylab="Count",
        col="cyan", main= 'Sentiment scores of Losers')

grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)

par(mfrow=c(1,1))


#CANDLESTICK FOR PDCO
getSymbols("PDCO",src='yahoo')

gf1 <- data.frame(Date=index(PDCO),coredata(PDCO))
gf1 <- with(gf1, gf1[(gf1$Date >= "2019-12-04" & gf1$Date <= "2019-12-10"),])

pdcocsc <- gvisCandlestickChart(gf1, xvar = "Date", low = "PDCO.Low", open = "PDCO.Open",
                                close = "PDCO.Close", high = "PDCO.High",
                                options= list (legend="top", height=400, width=500))
plot(pdcocsc)

#CANDLESTICK FOR ACAD
getSymbols("ACAD",src='yahoo')

gf2 <- data.frame(Date=index(ACAD),coredata(ACAD))
gf2 <- with(gf2, gf2[(gf2$Date >= "2019-12-04" & gf2$Date <= "2019-12-10"),])

acadcsc <- gvisCandlestickChart(gf2, xvar = "Date", low = "ACAD.Low", open = "ACAD.Open",
                                close = "ACAD.Close", high = "ACAD.High",
                                options= list (legend="top", height=400, width=500))
plot(acadcsc)

#CANDLESTICK FOR RH
getSymbols("RH",src='yahoo')

gf3 <- data.frame(Date=index(RH),coredata(RH))
gf3 <- with(gf3, gf3[(gf3$Date >= "2019-12-04" & gf3$Date <= "2019-12-10"),])

rhcsc <- gvisCandlestickChart(gf3, xvar = "Date", low = "RH.Low", open = "RH.Open",
                              close = "RH.Close", high = "RH.High",
                              options= list (legend="top", height=400, width=500))
plot(rhcsc)

#CANDLESTICK FOR SAGE
getSymbols("SAGE",src='yahoo')

df <- data.frame(Date=index(SAGE),coredata(SAGE))
df <- with(df, df[(df$Date >= "2019-12-04" & df$Date <= "2019-12-10"),])

sagecsc <- gvisCandlestickChart(df, xvar = "Date", low = "SAGE.Low", open = "SAGE.Open",
                                close = "SAGE.Close", high = "SAGE.High",
                                options= list (legend="top", height=400, width=500))
plot(sagecsc)

#CANDLESTICK FOR ESTC
getSymbols("ESTC",src='yahoo')

df1 <- data.frame(Date=index(ESTC),coredata(ESTC))
df1 <- with(df1, df1[(df1$Date >= "2019-12-04" & df1$Date <= "2019-12-10"),])

estccsc <- gvisCandlestickChart(df1, xvar = "Date", low = "ESTC.Low", open = "ESTC.Open",
                                close = "ESTC.Close", high = "ESTC.High",
                                options= list (legend="top", height=400, width=500))
plot(estccsc)


#CANDLESTICK FOR KOD
getSymbols("KOD",src='yahoo')

df2 <- data.frame(Date=index(KOD),coredata(KOD))
df2 <- with(df2, df2[(df2$Date >= "2019-12-04" & df2$Date <= "2019-12-10"),])

kodcsc <- gvisCandlestickChart(df2, xvar = "Date", low = "KOD.Low", open = "KOD.Open",
                               close = "KOD.Close", high = "KOD.High",
                               options= list (legend="top", height=400, width=500))
plot(kodcsc)


#Cummilative returns for 1$ invested on the stocks on 12-05-2019

#Calculating the daily returns for multiple stocks
tickers <- c("PDCO","ACAD","RH","SAGE","ESTC","KOD")

multpl_stocks <- tq_get(tickers,
                        from = "2019-12-05",
                        to = "2019-12-18",
                        get = "stock.prices")

multpl_stock_daily_returns <- multpl_stocks %>%
  group_by(symbol) %>%  # Need to group multiple stocks
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns')


multpl_stock_daily_returns %>%
  group_by(symbol) %>%  # Need to group multiple stocks
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cumulative_returns = cr - 1) %>%
  ggplot(aes(x = date, y = cumulative_returns+1, color = symbol)) +
  geom_line(size = 1) +
  labs(x = "Date", y = "Cumulative Returns") +
  ggtitle("Value of 1$ invested on 2019/12/05") +
  theme_bw()


