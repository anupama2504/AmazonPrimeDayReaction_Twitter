install.packages("twitteR")
install.packages("ROAuth")
install.packages("httr")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")

library(twitteR)
library(ROAuth)
library(httr)
library(ggplot2)

#setup the API keys
setwd('..\\TwitterProject')

#setup twitter app to get following keys
api_key <- "--------------------"
api_secret <- "------------"
access_token <- "--------------------------------"
access_token_secret <- "---------------------------------"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


#accessing reaction tweet for Amazon Prime day

hashtags_temp <- c('#primeday', '#primeday2018', '#primedayfail', '#primedaydeals', '#amazonprimeday',
              '#amazonprimedaysale', '#amazonprimedayfail', '#amazonprimeday2018', '#amazoncrashed')

hashtags <- paste(hashtags_temp, collapse = ' OR ')

primeDay_tweets <- searchTwitter(hashtags , n=2000)

#convert list to dataframe

primeDay_tweets_df <- twListToDF(primeDay_tweets)
#View(primeDay_tweets_df)

primeDay_tweets_df <- primeDay_tweets_df[, order(names(primeDay_tweets_df))] #ordering file with column names



#writing a function to calculate scores
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(dplyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
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
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

# gsub('[[:punct:]]'   '[[:cntrl:]]'    '\\d+' --- removes punctuations, control words and digits from tweets


#downloaded and used pos n negative word-list from github.

pos_dic = scan('wordDictionaryFiles/positive-words.txt',
               what='character', comment.char=';')
neg_dic = scan('wordDictionaryFiles/negative-words.txt',
               what='character', comment.char=';')
# Add a few twitter-specific negative phrases
pos.words <- c(pos_dic, 'upgrade', ':)', '#iVoted', 'voted')
neg.words <-  c(neg_dic,'wtf', 'epicfail', 'douchebag')

primeDay_tweets_df$text <- as.factor(primeDay_tweets_df$text)
tweets <- iconv(primeDay_tweets_df$text, "WINDOWS-1252","UTF-8")
scores <- score.sentiment(tweets, pos.words, neg.words, .progress='text')


#total evaluation and visualization: positive negative neutral

scores <- mutate(scores, tweet=ifelse(stat$score > 0, 'Positive', ifelse(scores$score < 0, 'Negative', 'Neutral')))
barplot(table(scores$tweet), main = "Reaction to Amazon Prime Day (Based on 2000 tweets)", ylim = c(0,1000) , xlab = "Reaction", ylab ="# Tweets", col = c("Red","light Blue","Green"))
