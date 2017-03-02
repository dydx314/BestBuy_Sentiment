# required pakacges
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)


#RESTART R session!

library(devtools)
install_github("twitteR", username="geoffjentry")

api_key <- "eKFISpvFsHsVP6mFMt7A9zSrv"

api_secret <- "MNybopaBNlxK66wS4N1QxSfpJIT6ymAI3oibFG5RzxM9jlXv31"

access_token <- "128727224-14pXnAab6Tcrq4TLOPNaHR29qI4pDYFxK5ZnQ0Wy"

access_token_secret <- "cmisiAA2WwSiXmhrVbRY9US8YfG8OpWrh1Dm6NfMb3feZ"

twitteR:::setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

analyse <- function(person, num) {
  some_tweets = twitteR:::searchTwitter(person, n=num, lang="en")
  some_txt = sapply(some_tweets, function(x) x$getText())
  
  # remove retweet entities
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  # remove at people
  some_txt = gsub("@\\w+", "", some_txt)
  # remove punctuation
  some_txt = gsub("[[:punct:]]", "", some_txt)
  # remove numbers
  some_txt = gsub("[[:digit:]]", "", some_txt)
  # remove html links
  some_txt = gsub("http\\w+", "", some_txt)
  # remove unnecessary spaces
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function 
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply 
  some_txt = sapply(some_txt, try.error)
  
  # remove NAs in some_txt
  some_txt = some_txt[!is.na(some_txt)]
  names(some_txt) = NULL
  
  
  # FIRST METHOD
  
  #text.corpus <- Corpus(VectorSource(some_txt))
  #text.corpus <- score.sentiment(text.corpus)
  
  
  
  
  # SECOND METHOD
  
  # classify emotion
  class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"
  
  # classify polarity
  class_pol = classify_polarity(some_txt, algorithm="bayes")
  # get polarity best fit
  polarity = class_pol[,4]
  
  # data frame with results
  sent_df = data.frame(text=some_txt, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)
  
  # sort data frame
  sent_df = within(sent_df,
                   emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  
  
  # RUN ONLY ONCE
  analysis <- c()
  for (i in unique(sent_df$emotion)) {
    if (i != "unknown")
      analysis <- rbind(analysis, c(i, sum(sent_df$emotion == i)))
  }
  analysis <- as.data.frame(analysis)
  analysis$V2  <- as.numeric(as.character(analysis$V2))
  #   disgust <- sum(sent_df$emotion == "disgust")
  #   fear <- sum(sent_df$emotion == "fear")
  #   anger <- sum(sent_df$emotion == "anger")
  #   joy <- sum(sent_df$emotion == "joy")
  #   sadness <- sum(sent_df$emotion == "sadness")
  #   surprise <- sum(sent_df$emotion == "surprise")
  
  #  donald_trump_analysis = data.frame(c("disgust","fear","anger","joy","sadness","surprise"),c(disgust,fear,anger,joy,sadness,surprise))
  ggplot(analysis, mapping = aes(V1, V2)) + geom_density() + ggtitle(person) + theme(plot.title = element_text(size=22))

  
}













# TESTING

some_tweets = twitteR:::searchTwitter("donald trump", n=500, lang="en")
some_txt = sapply(some_tweets, function(x) x$getText())

# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL


# FIRST METHOD

#text.corpus <- Corpus(VectorSource(some_txt))
#text.corpus <- score.sentiment(text.corpus)




# SECOND METHOD

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# RUN ONLY ONCE
analysis <- c()
for (i in unique(sent_df$emotion)) {
  if (i != "unknown")
    analysis <- rbind(analysis, c(i, sum(sent_df$emotion == i)))
}
analysis <- as.data.frame(analysis)
analysis$V2  <- as.numeric(as.character(analysis$V2))
#   disgust <- sum(sent_df$emotion == "disgust")
#   fear <- sum(sent_df$emotion == "fear")
#   anger <- sum(sent_df$emotion == "anger")
#   joy <- sum(sent_df$emotion == "joy")
#   sadness <- sum(sent_df$emotion == "sadness")
#   surprise <- sum(sent_df$emotion == "surprise")

#  donald_trump_analysis = data.frame(c("disgust","fear","anger","joy","sadness","surprise"),c(disgust,fear,anger,joy,sadness,surprise))
ggplot(analysis, mapping = aes(V1, V2)) + geom_density()
