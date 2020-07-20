library(ggplot2)
library(ggridges)
library(dplyr)
library(Hmisc)
library(reshape2)
library(zoo)
library(Matching)
library(ebal)
library(tm)
library(austin)

rm(list=ls())


# load data, to Corpus
df <-  read.csv('DE_text_20200526.csv',fileEncoding = 'UTF-8')
df <- df[df$n_words_raw > 50,]
textcorpus <- Corpus(VectorSource(df$raw)) # ;rm(df)

tdm <- TermDocumentMatrix(textcorpus, control = list(tolower=T, removePunctuation=T)); rm(textcorpus)

# get into wordfish shape
tdm <- as.matrix(tdm)
tdm <- as.wfm(tdm)
tdm <- as.wfm(trim(tdm))

tdm_train <- tdm[,sample(ncol(tdm), 1000)]

# remove 0 marginal sums from train and full matrix (ie terms that dont show up in the 1000 training docs)
tdm <- tdm[rowSums(tdm_train)!=0,]
tdm_train <- tdm_train[rowSums(tdm_train)!=0,]


# Wordscores
wf <- wordfish(tdm_train);rm(tdm_train)
# wf_sum <- summary(wf)


# # extract coefficients
coefs <- coef(wf)
write.csv(coefs[1], 'wordfish/coefs.csv')

# sort and look at top and bottom coefs:
coefs <- coefs[1] %>% 
  as.data.frame() 
coefs$word <- rownames(coefs)

most <- coefs %>% 
  arrange(words.beta) %>% 
  head(10) %>% 
  write.csv()
  
least <- coefs %>% 
  arrange(words.beta) %>% 
  tail(10)

write.csv(most, 'wordfish/most.csv')
write.csv(least, 'wordfish/least.csv')



# predict for all
estim <- predict(wf, newdata=tdm);rm(tdm, wf)
write.csv(estim, 'wordfish/estim.csv')






# restart here to clear working memory

## stopwords removed
library(ggplot2)
library(ggridges)
library(dplyr)
library(Hmisc)
library(reshape2)
library(zoo)
library(Matching)
library(ebal)
library(tm)
library(austin)
setwd('C:/Users/nicol/Dropbox/Studium/Amsterdam/Studies/Semester 4/Master Thesis/data/dataverse_files')

df <-  read.csv('DE_text_20200526.csv',fileEncoding = 'UTF-8')
df <- df[df$n_words_raw > 50,]
textcorpus <- Corpus(VectorSource(df$raw))
tdm <- TermDocumentMatrix(textcorpus, control = list(tolower=T, removePunctuation=T, stopwords=stopwords('german'))); rm(textcorpus)
tdm <- as.matrix(tdm)
tdm <- as.wfm(tdm)
tdm <- as.wfm(trim(tdm))
tdm_train <- tdm[,sample(ncol(tdm), 1000)]
tdm <- tdm[rowSums(tdm_train)!=0,]
tdm_train <- tdm_train[rowSums(tdm_train)!=0,]


# Wordscores
wf_nosw <- wordfish(tdm_train);rm(tdm_train)
estim_nosw <- predict(wf_nosw, newdata=tdm);rm(tdm, wf_nosw)
write.csv(estim_nosw, 'wordfish/estim_nosw.csv')


# restart here


## no trimming
library(ggplot2)
library(ggridges)
library(dplyr)
library(Hmisc)
library(reshape2)
library(zoo)
library(Matching)
library(ebal)
library(tm)
library(austin)
setwd('C:/Users/nicol/Dropbox/Studium/Amsterdam/Studies/Semester 4/Master Thesis/data/dataverse_files')

df <-  read.csv('DE_text_20200526.csv',fileEncoding = 'UTF-8')
df <- df[df$n_words_raw > 50,]
textcorpus <- Corpus(VectorSource(df$raw))
tdm <- TermDocumentMatrix(textcorpus, control = list(tolower=T, removePunctuation=T)); rm(textcorpus);#gc()
tdm <- as.matrix(tdm)
tdm <- as.wfm(tdm)
# tdm <- as.wfm(trim(tdm))
tdm_train <- tdm[,sample(ncol(tdm), 1000)]
tdm <- tdm[rowSums(tdm_train)!=0,]
tdm_train <- tdm_train[rowSums(tdm_train)!=0,]


# Wordscores
wf_notrim <- wordfish(tdm_train)
estim_notrim <- predict(wf_notrim, newdata=tdm);rm(tdm, wf_notrim)
write.csv(estim_notrim, 'wordfish/estim_notrim.csv')




# merge with df
df <-  read.csv('DE_notext_20200526.csv',fileEncoding = 'UTF-8')
df <- df[df$n_words_raw > 50,]
df$wordfish <- read.csv('wordfish/estim.csv')[,2]
df$wordfish_nosw <- read.csv('wordfish/estim_nosw.csv')[,2]
df$wordfish_notrim <- read.csv('wordfish/estim_notrim.csv')[,2]

# fix dates, min monthly date, save
df$date <- as.Date(df$date, '%Y-%m-%d')
df$my <- lubridate::floor_date(df$date, "month")
write.csv(df, 'DE_wordfish.csv')

