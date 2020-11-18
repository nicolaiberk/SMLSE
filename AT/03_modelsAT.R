library(dplyr)
library(lubridate)
library(Hmisc)
library(ggplot2)
library(gridExtra)

polls <- read.csv('raw/at_final.csv') # obtained from Neuwal.at
pps <- read.csv('smlse/AT_notext.csv')

# create monthly averages for FPOE polling & pred probs ####
## polls ####

# wide -> long
p1party <- polls$p1Css
p2party <- polls$p2Css
p3party <- polls$p3Css
p4party <- polls$p4Css
p5party <- polls$p5Css
p6party <- polls$p6Css

p1value <- polls$p1Value
p2value <- polls$p2Value
p3value <- polls$p3Value
p4value <- polls$p4Value
p5value <- polls$p5Value
p6value <- polls$p6Value

polls_long <- as.data.frame(cbind(c(p1party, p2party, p3party, p4party, p5party, p6party), 
                                  c(p1value, p2value, p3value, p4value, p5value, p6value),
                                  polls$datum,
                                  polls$n))


polls_long$V2 <- as.numeric(polls_long$V2)
polls_long$V3 <- as.Date(polls_long$V3, format = '%Y-%m-%d')
polls_long$V4 <- as.numeric(polls_long$V4)


colnames(polls_long) <- c('party', 'value', 'date', 'n')

# filter fpoe
polls_fp <- polls_long %>% 
  filter(party == 'fpo')


# aggregate
polls_fp$my <- floor_date(polls_fp$date, "month")
polls_agg <- polls_fp %>% 
  select(value, my, n) %>% 
  group_by(my) %>% 
  summarise(mean_poll = mean(value),
            wtd_mean_poll = wtd.mean(x=value, w = n))


p1 <- ggplot(data = polls_agg, aes(x = my, y = mean_poll)) + geom_line()
p2 <- ggplot(data = polls_agg, aes(x = my, y = wtd_mean_poll)) + geom_line()
grid.arrange(p1,p2)
cor(polls_agg$mean_poll, polls_agg$wtd_mean_poll) #.997


## pps ####
pps$my <- floor_date(as.Date(pps$date, format = '%Y-%m-%d'), "month")
pps_agg <- pps %>% 
  filter(party == 'FPÃ–') %>% 
  select(RR_pred, my, n_words) %>% 
  group_by(my) %>% 
  summarise(wtd_mean_pred = wtd.mean(x=RR_pred, w = n_words))


## merge ####
pps_agg$date_str <- as.character(pps_agg$my)
polls_agg$date_str <- as.character(polls_agg$my)

at_merged <- merge(pps_agg, polls_agg, by = 'date_str', all.x = T)


# diff to ensure stationarity
at_merged$diff_pred <- c(NA, diff(at_merged$wtd_mean_pred))
at_merged$diff_poll <- c(NA, diff(at_merged$wtd_mean_poll))


# time-series model ####

## granger causality test
library(lmtest)
g1 <- grangertest(at_merged$diff_pred ~ at_merged$diff_poll, order = 3) # not significant
g2 <- grangertest(at_merged$diff_poll ~ at_merged$diff_pred, order = 3) # not significant

g3 <- grangertest(at_merged$diff_pred ~ at_merged$diff_poll, order = 12) # not significant
g4 <- grangertest(at_merged$diff_poll ~ at_merged$diff_pred, order = 12) # not significant

p1 <- ggplot(at_merged, aes(x=my.x, y = wtd_mean_poll)) +geom_line()
p2 <- ggplot(at_merged, aes(x=my.x, y = wtd_mean_pred)) +geom_line() +geom_vline(xintercept= c(as.Date('2008-09-28'), as.Date('2013-09-29'), as.Date('2017-10-15')))
grid.arrange(p1,p2)



# regression model with three lags & government variable

## define government variable
at_merged$gov <- 0
at_merged$gov[at_merged$my.x > as.Date('2017-10-15')] <- 1
at_merged$diff_gov <- c(NA, diff(at_merged$gov))

## define lags
library(tis)
at_merged$diff_pred_l1 <- lag(at_merged$diff_pred, 1)
at_merged$diff_pred_l2 <- lag(at_merged$diff_pred, 2)
at_merged$diff_pred_l3 <- lag(at_merged$diff_pred, 3)



## diffed models
m1 <- lm(at_merged$diff_poll ~ at_merged$diff_pred) # no effect
m2 <- lm(at_merged$diff_poll ~ at_merged$diff_pred + at_merged$diff_gov) # no effect
m3 <- lm(at_merged$diff_poll ~ at_merged$diff_pred_l1 + at_merged$diff_pred_l2 + at_merged$diff_pred_l3) # neg effect off 3rd lag
m4 <- lm(at_merged$diff_poll ~ at_merged$diff_pred_l1 + at_merged$diff_pred_l2 + at_merged$diff_pred_l3 + at_merged$diff_gov) # neg effect off 3rd lag
m5 <- lm(at_merged$diff_poll ~ at_merged$diff_pred + at_merged$diff_pred_l1 + at_merged$diff_pred_l2 + at_merged$diff_pred_l3 + at_merged$diff_gov) # neg effect of 3rd lag
m6 <- lm(at_merged$diff_poll ~ at_merged$diff_pred_l3) # neg effect of 3rd lag

## save table
table <- stargazer::stargazer(# m1,m2,
                              m6,
                              #m4,
                              m5,m3)

fileConn<-file("polls_ols.txt")
writeLines(table, fileConn)
close(fileConn)

# filter out gov participation period
at_merged <- at_merged %>% 
  filter(my.x < as.Date('2017-10-15'))


g5 <- grangertest(at_merged$diff_pred ~ at_merged$diff_poll, order = 3) # not significant
g6 <- grangertest(at_merged$diff_poll ~ at_merged$diff_pred, order = 3) # significant p<0.05

g7 <- grangertest(at_merged$diff_pred ~ at_merged$diff_poll, order = 12) # not significant
g8 <- grangertest(at_merged$diff_poll ~ at_merged$diff_pred, order = 12) # not significant

p1 <- ggplot(at_merged, aes(x=my.x, y = wtd_mean_poll)) +geom_line()
p2 <- ggplot(at_merged, aes(x=my.x, y = wtd_mean_pred)) +geom_line() +geom_vline(xintercept= c(as.Date('2008-09-28'), as.Date('2013-09-29'), as.Date('2017-10-15')))
grid.arrange(p1,p2)


## for oevp ####
pps_vp <- pps %>% 
  filter(party == 'Ã–VP') %>% 
  select(RR_pred, my, n_words) %>% 
  group_by(my) %>% 
  summarise(wtd_mean_pred = wtd.mean(x=RR_pred, w = n_words))


## merge ####
pps_vp$date_str <- as.character(pps_vp$my)
polls_agg$date_str <- as.character(polls_agg$my)

at_merged <- merge(pps_vp, polls_agg, by = 'date_str', all.x = T)

## diff to ensure stationarity
at_merged$diff_pred <- c(NA, diff(at_merged$wtd_mean_pred))
at_merged$diff_poll <- c(NA, diff(at_merged$wtd_mean_poll))


grangertest(at_merged$diff_pred ~ at_merged$diff_poll, order = 3) # not significant
grangertest(at_merged$diff_poll ~ at_merged$diff_pred, order = 3) # not significant

grangertest(at_merged$diff_pred ~ at_merged$diff_poll, order = 12) # not significant
grangertest(at_merged$diff_poll ~ at_merged$diff_pred, order = 12) # not significant

p1 <- ggplot(at_merged, aes(x=my.x, y = wtd_mean_poll)) +geom_line()
p2 <- ggplot(at_merged, aes(x=my.x, y = wtd_mean_pred)) +geom_line()
grid.arrange(p1,p2) # does not respond to increasing performance in polling, only after leadership challenge

# filter out gov participation period
at_merged <- at_merged %>% 
  filter(my.x < as.Date('2017-10-15'))
grangertest(at_merged$diff_pred ~ at_merged$diff_poll, order = 3) # not significant
grangertest(at_merged$diff_poll ~ at_merged$diff_pred, order = 3) # not significant

grangertest(at_merged$diff_pred ~ at_merged$diff_poll, order = 12) # not significant
grangertest(at_merged$diff_poll ~ at_merged$diff_pred, order = 12) # not significant



##############################################
# t-test party distance gov-opp nl

## FP distance to VP
fp_pred <- pps %>% 
  filter(party == 'FPÃ–') %>% 
  select(Ã.VP_pred, my, n_words) %>% 
  group_by(my) %>% 
  summarise(fp_pred = wtd.mean(x=Ã.VP_pred, w = n_words))


vp_pred <- pps %>% 
  filter(party == 'Ã–VP') %>% 
  select(FPÃ._pred, my, n_words) %>% 
  group_by(my) %>% 
  summarise(vp_pred = wtd.mean(x=FPÃ._pred, w = n_words))

vpfp <- merge(fp_pred, vp_pred, by = 'my', all.x = T)

vpfp$gov <- 0
vpfp$gov[vpfp$my >= as.Date('01.02.2000', format = '%d.%m.%Y') & vpfp$my <= as.Date('30.09.2006', format = '%d.%m.%Y')] <- 1
vpfp$gov[vpfp$my >= as.Date('01.12.2018', format = '%d.%m.%Y') & vpfp$my <= as.Date('30.05.2019', format = '%d.%m.%Y')] <- 1

t.test(vpfp$fp_pred ~ vpfp$gov) # significant, p < 0.000001, diff = 0.271- 0.155 = 0.116
t.test(vpfp$vp_pred ~ vpfp$gov) # significant, p < 0.000001, diff = 0.27 - 0.159 = 0.111
