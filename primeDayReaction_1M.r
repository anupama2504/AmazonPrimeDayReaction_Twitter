install.packages("twitteR")
install.packages("ROAuth")
install.packages("httr")
install.packages("dplyr")
install.packages("stringr")
install.packages("plyr")

library(twitteR)
library(ROAuth)
library(httr)
library(dplyr)
library(stringr)
library(plyr)

#setup the API keys
setwd('C:\\Users\\anupama25\\Desktop\\TwitterProject')
api_key <- "-----------------------------"
api_secret <- "-------------------------"
access_token <- "--------------------------"
access_token_secret <- "------------------------------"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


#accessing reaction tweet for Amazon Prime day

hashtags_temp <- c('#primeday', '#primeday2018', '#primedayfail', '#primedaydeals', '#amazonprimeday',
              '#amazonprimedaysale', '#amazonprimedayfail', '#amazonprimeday2018', '#amazoncrashed')

hashtags <- paste(hashtags_temp, collapse = ' OR ')

primeDay_tweets <- searchTwitter(hashtags , since = '2018-07-15', until = '2018-07-17', n=1000000)
#convert list to dataframe

primeDay_tweets_df <- twListToDF(primeDay_tweets)
#View(primeDay_tweets_df)

primeDay_tweets_df <- primeDay_tweets_df[, order(names(primeDay_tweets_df))] #ordering file with column names


#for creating cumulative file
#temp <- strftime(primeDay_tweets_df$created, '%Y-%m-%d')
if (file.exists(paste('primeDay', '_stack.csv'))== FALSE) {
  write.csv(primeDay_tweets_df, file = paste('primeDay', '_stack.csv'), row.names = FALSE)
}




#writing a function to calculate scores
score.sentiment <- function(sentences, pos.words, neg.words, myprogress='text')
{
  require(dplyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words, myprogress='text'){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence) 
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

#downloaded and used pos n negative word-list from github.

pos_dic = scan('wordDictionaryFiles/positive-words.txt',
               what='character', comment.char=';')
neg_dic = scan('wordDictionaryFiles/negative-words.txt',
               what='character', comment.char=';')
# Add a few twitter-specific negative phrases
pos.words1 <- c(pos_dic, 'upgrade', ':)', '#iVoted', 'voted')
neg.words1 <- c(neg_dic,  'wtf', 'epicfail', 'douchebag')

primeDay_tweets_df$text <- as.factor(primeDay_tweets_df$text)
tweets <- iconv(primeDay_tweets_df$text, "WINDOWS-1252","UTF-8")
#scores <- score.sentiment(tweets, pos.words1, neg.words1, myprogress='text')
scores <- score.sentiment(tweets, pos.words1, neg.words1, myprogress='text')


#total evaluation and visualization: positive negative neutral

scores <- mutate(scores, tweet=ifelse(scores$score > 0, 'Positive', ifelse(scores$score < 0, 'Negative', 'Neutral')))
temp <- summary(as.factor(scores$tweet))
Percentage <- temp/158093 *100

bp <- barplot(table(scores$tweet), main = "Reaction Based on PrimeDay tweets",
        ylim = c(0,99000) ,
        xlab = "Reaction", 
        ylab ="# Tweets", 
        col = c("Red","light Blue","Green"))
text(bp,0, paste(round(Percentage,1),'%'),adj = c(0,0), cex=1, pos = 3)
