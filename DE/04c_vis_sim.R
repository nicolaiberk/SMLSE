# assess similarity scores

setwd('C:/Users/nicol/Dropbox/Studium/Amsterdam/Studies/Semester 4/Master Thesis/data/dataverse_files')

library(dplyr)
library(ggplot2)
library(ggridges)
library(corrplot)
library(psych)
library(Hmisc)

df <- read.csv('DE_similarities.csv')
df$party <- factor(df$party, levels = c("AfD", "CDU/CSU", "SPD", "FDP", "GRUENE", "PDS/LINKE", "independent"))
partycols=c("lightblue", "grey", "#FF6565", "gold", "lightgreen", "firebrick", 'blue')

df$afd_pred_log <- log(df$afd_pred)
df$afd[df$afd == 'False'] <- 0
df$afd[df$afd == 'True'] <- 1
df$afd <- as.numeric(df$afd)


## calculate group etimates
df_pt <- df %>% 
  select(party, afd_pred, cosine_sim, jaccard, n_words_raw) %>% 
  group_by(party) %>% 
  summarise(mean_cs  = wtd.mean(cosine_sim, weights = n_words_raw), 
            mean_j   = wtd.mean(jaccard, weights = n_words_raw),
            mean_est = wtd.mean(afd_pred, weights = n_words_raw),
            sd_cs  = sqrt(wtd.var(x=cosine_sim, weights=n_words_raw, normwt = T)), 
            sd_j   = sqrt(wtd.var(x=jaccard,    weights=n_words_raw, normwt = T)),
            sd_est = sqrt(wtd.var(x=afd_pred,   weights=n_words_raw, normwt = T)),
            n_speeches = length(party))


df_pt$se_est <- df_pt$sd_est/sqrt(df_pt$n_speeches)
df_pt$ci_low_est <- df_pt$mean_est - 1.96*df_pt$se_est
df_pt$ci_up_est <- df_pt$mean_est + 1.96*df_pt$se_est

df_pt$se_cs <- df_pt$sd_cs/sqrt(df_pt$n_speeches)
df_pt$ci_low_cs <- df_pt$mean_cs - 1.96*df_pt$se_cs
df_pt$ci_up_cs <- df_pt$mean_cs + 1.96*df_pt$se_cs

df_pt$se_j <- df_pt$sd_j/sqrt(df_pt$n_speeches)
df_pt$ci_low_j <- df_pt$mean_j - 1.96*df_pt$se_j
df_pt$ci_up_j <- df_pt$mean_j + 1.96*df_pt$se_j



## plot
p0 <- ggplot(df, aes(x = afd_pred, y = party, fill = party)) + 
  #geom_density_ridges(alpha = 0.2, show.legend = F) + 
  geom_linerange(inherit.aes = F, data = df_pt, aes(xmin = ci_low_est, xmax = ci_up_est, y = party)) +
  geom_point(inherit.aes = F, data = df_pt, aes(x = mean_est, y = party)) + 
  xlab('') + ylab('') + ggtitle('SMLSE') + scale_fill_manual(values=partycols)

p1 <- ggplot(df, aes(x = cosine_sim, y = party, fill = party)) + 
 # geom_density_ridges(alpha = 0.2, show.legend = F) + 
  geom_linerange(inherit.aes = F, data = df_pt, aes(xmin = ci_low_cs, xmax = ci_up_cs, y = party)) +
  geom_point(inherit.aes = F, data = df_pt, aes(x = mean_cs, y = party)) + 
  xlab('') + ylab('') + ggtitle('Cosine Similarity') + scale_fill_manual(values=partycols)

# p2 <- ggplot(df, aes(x = cosine_sim_alt, y = party, fill = party)) + 
#   geom_density_ridges(alpha = 0.2, show.legend = F) + 
#   xlab('') + ylab('') + ggtitle('Cosine Similarity Alternative I') + scale_fill_manual(values=partycols)
# 
# p3 <- ggplot(df, aes(x = cosine_sim_alt2, y = party, fill = party)) + 
#   geom_density_ridges(alpha = 0.2, show.legend = F) + 
#   xlab('') + ylab('') + ggtitle('Cosine Similarity Alternative II') + scale_fill_manual(values=partycols)

p4 <- ggplot(df, aes(x = jaccard, y = party, fill = party)) + 
#  geom_density_ridges(alpha = 0.2, show.legend = F) + 
  geom_linerange(inherit.aes = F, data = df_pt, aes(xmin = ci_low_j, xmax = ci_up_j, y = party)) +
  geom_point(inherit.aes = F, data = df_pt, aes(x = mean_j, y = party)) + 
  xlab('') + ylab('') + ggtitle('Jaccard Similarity') + scale_fill_manual(values=partycols)



png('vis/sim_measures/paper_corr.png', height = 15, width = 15, unit = 'cm', res = 1200)
df %>% 
  select(cosine_sim, jaccard, afd_pred_log) %>% 
  pairs.panels( 
             method = "pearson", # correlation method
             hist.col = "lightblue",
             density = T,  # show density plots
             ellipses = F # show correlation ellipses
  ) 
dev.off()

png('vis/sim_measures/all_corr.png', height = 20, width = 30, unit = 'cm', res = 1200)
df %>% 
  select(cosine_sim, cosine_sim_alt, cosine_sim_alt2, jaccard, afd_pred, afd_pred_log, afd) %>% 
  pairs.panels(
    method = "pearson", # correlation method
    hist.col = "#00AFBB",
    density = T,  # show density plots
    ellipses = F # show correlation ellipses
  )
dev.off()


# add wordfish
df <-  read.csv('DE_notext_20200526.csv',fileEncoding = 'UTF-8')
df <- df[df$n_words_raw > 50,]
df$wordfish <- read.csv('wordfish/estim.csv')[,2]
df$party <- factor(df$party, levels = c("AfD", "CDU/CSU", "SPD", "FDP", "GRUENE", "PDS/LINKE", "independent"))


df_pt <- df %>% 
  select(party, wordfish, n_words_raw) %>% 
  group_by(party) %>% 
  summarise(mean_wf  = wtd.mean(wordfish, weights = n_words_raw), 
            sd_wf  = sqrt(wtd.var(x=wordfish, weights=n_words_raw, normwt = T)), 
            n_speeches = length(party))


df_pt$se_wf <- df_pt$sd_wf/sqrt(df_pt$n_speeches)
df_pt$ci_low_wf <- df_pt$mean_wf - 1.96*df_pt$se_wf
df_pt$ci_up_wf <- df_pt$mean_wf + 1.96*df_pt$se_wf


p5 <- ggplot(df, aes(x = wordfish, y = party, fill = party)) + 
  #  geom_density_ridges(alpha = 0.2, show.legend = F) + 
  geom_linerange(inherit.aes = F, data = df_pt, aes(xmin = ci_low_wf, xmax = ci_up_wf, y = party)) +
  geom_point(inherit.aes = F, data = df_pt, aes(x = mean_wf, y = party)) + 
  xlab('') + ylab('') + ggtitle('WORDFISH') + scale_fill_manual(values=partycols)


gridExtra::grid.arrange(p0, p1,
                        #p2,p3, 
                        # p4, 
                        p5, ncol = 1) %>% 
  ggsave(filename = 'vis/sim_measures/paper_pts.png')



#### WORDFISH ####


# correlate with pred probs
df <- read.csv('DE_wordfish.csv')
cor(df$wordfish, df$afd_pred)
cor(df$wordfish_nosw, df$afd_pred)
cor(df$wordfish_notrim, df$afd_pred)

# re-arrange parties, define color scheme
df$party <- factor(df$party, levels = c("AfD", "CDU/CSU", "SPD", "FDP", "GRUENE", "PDS/LINKE", "independent"))
partycols = c('blue', 'black', 'red', 'gold', 'green', 'firebrick', 'lightblue')

# aggregate per party
df_pt <- df %>%
  group_by(party) %>% 
  summarise(wf = wtd.mean(wordfish, n_words_raw),
            wf_nosw = wtd.mean(wordfish_nosw, n_words_raw),
            wf_notrim = wtd.mean(wordfish_notrim, n_words_raw),
            sd_wf = sqrt(wtd.var(x=wordfish, weights=n_words_raw, normwt = T)),
            sd_wf_nosw = sqrt(wtd.var(x=wordfish_nosw, weights=n_words_raw, normwt = T)),
            sd_wf_notrim = sqrt(wtd.var(x=wordfish_notrim, weights=n_words_raw, normwt = T)),
            pred = wtd.mean(afd_pred, n_words_raw),
            sd_pred = sqrt(wtd.var(x=afd_pred, weights=n_words_raw, normwt = T)),
            n_speeches = length(n_words_raw))

df_pt$wf_se <- df_pt$sd_wf/sqrt(df_pt$n_speeches)
df_pt$wf_ci_low <- df_pt$wf - 1.96*df_pt$wf_se
df_pt$wf_ci_up <- df_pt$wf + 1.96*df_pt$wf_se

df_pt$wf_nosw_se <- df_pt$sd_wf_nosw/sqrt(df_pt$n_speeches)
df_pt$wf_nosw_ci_low <- df_pt$wf_nosw - 1.96*df_pt$wf_nosw_se
df_pt$wf_nosw_ci_up <- df_pt$wf_nosw + 1.96*df_pt$wf_nosw_se

df_pt$wf_notrim_se <- df_pt$sd_wf_notrim/sqrt(df_pt$n_speeches)
df_pt$wf_notrim_ci_low <- df_pt$wf_notrim - 1.96*df_pt$wf_notrim_se
df_pt$wf_notrim_ci_up <- df_pt$wf_notrim + 1.96*df_pt$wf_notrim_se

df_pt$pred_se <- df_pt$sd_pred/sqrt(df_pt$n_speeches)
df_pt$pred_ci_low <- df_pt$pred - 1.96*df_pt$pred_se
df_pt$pred_ci_up <- df_pt$pred + 1.96*df_pt$pred_se





# plot distributions
party_dens_wf <- ggplot(df, aes(x=wordfish, y=party, fill = party)) + 
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=df_pt, aes(x = wf, y=party), show.legend = F)+
  geom_linerange(data=df_pt, aes(xmin = wf_ci_low, xmax = wf_ci_up, y=party), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  xlab('Score')+
  ggtitle('Wordfish')

party_dens_wf_nosw <- ggplot(df, aes(x=wordfish_nosw, y=party, fill = party)) + 
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=df_pt, aes(x = wf_nosw, y=party), show.legend = F)+
  geom_linerange(data=df_pt, aes(xmin = wf_nosw_ci_low, xmax = wf_nosw_ci_up, y=party), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  xlab('Score')+
  ggtitle('Wordfish (no stopwords)')

party_dens_wf_notrim <- ggplot(df, aes(x=wordfish_notrim, y=party, fill = party)) + 
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=df_pt, aes(x = wf_notrim, y=party), show.legend = F)+
  geom_linerange(data=df_pt, aes(xmin = wf_notrim_ci_low, xmax = wf_notrim_ci_up, y=party), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  xlab('Score')+
  ggtitle('Wordfish (all terms)')

party_dens_pred <- ggplot(df, aes(x=afd_pred, y=party, fill = party)) + 
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=df_pt, aes(x = pred, y=party), show.legend = F)+
  geom_linerange(data=df_pt, aes(xmin = pred_ci_low, xmax = pred_ci_up, y=party), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  xlab('Score')+
  ggtitle('SMLSE')


party_dens <- gridExtra::grid.arrange(party_dens_wf, party_dens_pred)
ggsave('vis/wordfish_pt_dens.png', party_dens)

party_dens <- gridExtra::grid.arrange(party_dens_wf, party_dens_wf_nosw, party_dens_wf_notrim, party_dens_pred)
ggsave('vis/wordfish_pt_dens_all.png', party_dens)



# look at party similarities across time 
df$date <- as.Date(df$date, '%Y-%m-%d')
df$my <- lubridate::floor_date(df$date, "month")


df_pt_my <- df %>%
  group_by(party, my) %>% 
  summarise(wf = wtd.mean(wordfish, n_words_raw),
            wf_nosw = wtd.mean(wordfish_nosw, n_words_raw),
            wf_notrim = wtd.mean(wordfish_notrim, n_words_raw),
            pred = wtd.mean(afd_pred, n_words_raw),
            n_speeches = length(n_words_raw))


p1a <- ggplot(df_pt_my, aes(x=my, y=wf, col = party)) + 
  geom_line() +
  scale_color_manual(values = partycols)+
  ylab('Score')+
  ggtitle('Wordfish')

p1b <- ggplot(df_pt_my, aes(x=my, y=wf_nosw, col = party)) + 
  geom_line() +
  scale_color_manual(values = partycols)+
  ylab('Score')+
  ggtitle('Wordfish (no stopwords)')

p1c <- ggplot(df_pt_my, aes(x=my, y=wf_notrim, col = party)) + 
  geom_line() +
  scale_color_manual(values = partycols)+
  ylab('Score')+
  ggtitle('Wordfish (all terms)')


p2 <- ggplot(df_pt_my, aes(x=my, y=pred, col = party)) + 
  geom_line() +
  scale_color_manual(values = partycols)+
  ylab('Score')+
  ggtitle('"SMLSE"')


full <- gridExtra::grid.arrange(p1a,p2)
ggsave('vis/wordfish_pt_my.png', full)

full <- gridExtra::grid.arrange(p1a,p1b,p1c,p2)
ggsave('vis/wordfish_pt_my_all.png', full)

# speaker distribution

## aggregate
df_sp <- df %>% 
  group_by(speaker, party) %>% 
  summarise(pred = weighted.mean(x=afd_pred, w=n_words_raw),
            wf = weighted.mean(x=wordfish, w=n_words_raw),
            n_speeches = length(afd_pred),
            n_words = sum(n_words_raw))

## plot
speakers_wf <-
  ggplot(df_sp[df_sp$n_words>100,],aes(x=party, y=wf, fill=party)) + 
  geom_dotplot(binwidth=0.1, method='histodot', drop = T, stackratio = .7, binaxis = 'y', stackdir = 'center', show.legend = F)+
  scale_fill_manual(values=partycols) +
  theme_minimal() +
  xlab('')+
  ylab('Score')+
  ggtitle('Wordfish')

speakers_pred <-
  ggplot(df_sp[df_sp$n_words>100,],aes(x=party, y=pred, fill=party)) + 
  geom_dotplot(binwidth=0.0275, method='histodot', drop = T, stackratio = .7, binaxis = 'y', stackdir = 'center', show.legend = F)+
  scale_fill_manual(values=partycols) +
  theme_minimal() +
  xlab('')+
  ylab('Score')+
  ggtitle('SMLSE')

speakers_comp <- gridExtra::grid.arrange(speakers_wf, speakers_pred)
ggsave('vis/wordfish_speakers.png', speakers_comp)

