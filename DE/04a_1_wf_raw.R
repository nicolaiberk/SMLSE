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


# load data, to Corpus
df <-  read.csv('smlse/DE_text.csv',fileEncoding = 'UTF-8')
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


# Run wordfish & measure runtime
start_time <- Sys.time()
wf <- wordfish(tdm_train);rm(tdm_train)
end_time <- Sys.time()

diff = end_time - start_time



fileConn<-file("runtime_wf.txt")
writeLines(c("Start time: ", as.String(start_time), '\n', 'End Time: ', as.String(end_time), '\n\n', 'Difference: ', diff), fileConn)
close(fileConn)



# # extract coefficients
coefs <- coef(wf)
write.csv(coefs[1], 'vis/coefs.csv')

# sort and look at top and bottom coefs:
coefs <- coefs[1] %>% 
  as.data.frame() 
coefs$word <- rownames(coefs)

most <- coefs %>% 
  arrange(words.beta) %>% 
  head(10)
  
least <- coefs %>% 
  arrange(words.beta) %>% 
  tail(10)

write.csv(most, 'vis/most.csv')
write.csv(least, 'vis/least.csv')



# predict for all
estim <- predict(wf, newdata=tdm);rm(tdm, wf)
write.csv(estim, 'wordfish/estim.csv')