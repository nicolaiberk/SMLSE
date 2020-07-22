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

df <-  read.csv('smlse/DE_text.csv',fileEncoding = 'UTF-8')
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
df <-  read.csv('smlse/DE_notext.csv',fileEncoding = 'UTF-8')
df <- df[df$n_words_raw > 50,]
df$wordfish <- read.csv('wordfish/estim.csv')[,2]
df$wordfish_nosw <- read.csv('wordfish/estim_nosw.csv')[,2]
df$wordfish_notrim <- read.csv('wordfish/estim_notrim.csv')[,2]

# fix dates, min monthly date, save
df$date <- as.Date(df$date, '%Y-%m-%d')
df$my <- lubridate::floor_date(df$date, "month")
write.csv(df, 'wordfish/DE_wordfish.csv')
