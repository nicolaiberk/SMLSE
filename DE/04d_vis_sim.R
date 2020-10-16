library(dplyr)
library(ggplot2)
library(ggridges)
library(corrplot)
library(psych)
library(Hmisc)
# library(BBmisc)
library(RColorBrewer)


# load similarity scores
cosine_sim <- read.csv('DE/sims/DE_similarities.csv')
smlse_1000 <- read.csv('DE/smlse/DE_smlse1000.csv')
wordfish <- read.csv('DE/sims/DE_wordfish.csv')


cor(cosine_sim$n_words_raw, cosine_sim$cosine_sim) # 0.87 - mainly higher scores due to higher likelihood of words to be included - report


# normalise
# cosine_sim$afd_pred <- normalize(cosine_sim$afd_pred)
# cosine_sim$cosine_sim <- normalize(cosine_sim$cosine_sim)
# cosine_sim$jaccard <- normalize(cosine_sim$jaccard)
# 
# smlse_1000$afd_pred <- normalize(smlse_1000$afd_pred)
# 
# wordfish$wordfish <- normalize(wordfish$wordfish)

# aggregate cosine
cosine_sim <- cosine_sim %>% 
  select(party, afd_pred, cosine_sim, jaccard, n_words_raw) %>% 
  group_by(party) %>% 
  summarise(mean_cs  = wtd.mean(cosine_sim, weights = n_words_raw), 
            mean_j   = wtd.mean(jaccard, weights = n_words_raw),
            mean_est = wtd.mean(afd_pred, weights = n_words_raw),
            sd_cs  = sqrt(wtd.var(x=cosine_sim, weights=n_words_raw, normwt = T)), 
            sd_j   = sqrt(wtd.var(x=jaccard,    weights=n_words_raw, normwt = T)),
            sd_est = sqrt(wtd.var(x=afd_pred,   weights=n_words_raw, normwt = T)),
            n_speeches = length(party))


cosine_sim$se_est <- cosine_sim$sd_est/sqrt(cosine_sim$n_speeches)
cosine_sim$ci_low_est <- cosine_sim$mean_est - 1.96*cosine_sim$se_est
cosine_sim$ci_up_est <- cosine_sim$mean_est + 1.96*cosine_sim$se_est

cosine_sim$se_cs <- cosine_sim$sd_cs/sqrt(cosine_sim$n_speeches)
cosine_sim$ci_low_cs <- cosine_sim$mean_cs - 1.96*cosine_sim$se_cs
cosine_sim$ci_up_cs <- cosine_sim$mean_cs + 1.96*cosine_sim$se_cs

cosine_sim$se_j <- cosine_sim$sd_j/sqrt(cosine_sim$n_speeches)
cosine_sim$ci_low_j <- cosine_sim$mean_j - 1.96*cosine_sim$se_j
cosine_sim$ci_up_j <- cosine_sim$mean_j + 1.96*cosine_sim$se_j



# aggregate smlse 1000
smlse_1000 <- smlse_1000 %>% 
  select(party, afd_pred, n_words_raw) %>% 
  group_by(party) %>% 
  summarise(mean_1000 = wtd.mean(afd_pred, weights = n_words_raw),
            sd_est = sqrt(wtd.var(x=afd_pred,   weights=n_words_raw, normwt = T)),
            n_speeches = length(party))


smlse_1000$se_1000 <- smlse_1000$sd_est/sqrt(smlse_1000$n_speeches)
smlse_1000$ci_low_1000 <- smlse_1000$mean_1000 - 1.96*smlse_1000$se_1000
smlse_1000$ci_up_1000 <- smlse_1000$mean_1000 + 1.96*smlse_1000$se_1000


# aggregate wordfish
wordfish <- wordfish %>%
  group_by(party) %>% 
  summarise(wf = wtd.mean(wordfish, n_words_raw),
            wf_nosw = wtd.mean(wordfish_nosw, n_words_raw),
            wf_notrim = wtd.mean(wordfish_notrim, n_words_raw),
            sd_wf = sqrt(wtd.var(x=wordfish, weights=n_words_raw, normwt = T)),
            sd_wf_nosw = sqrt(wtd.var(x=wordfish_nosw, weights=n_words_raw, normwt = T)),
            sd_wf_notrim = sqrt(wtd.var(x=wordfish_notrim, weights=n_words_raw, normwt = T)),
            n_speeches = length(n_words_raw))

wordfish$wf_se <- wordfish$sd_wf/sqrt(wordfish$n_speeches)
wordfish$wf_ci_low <- wordfish$wf - 1.96*wordfish$wf_se
wordfish$wf_ci_up <- wordfish$wf + 1.96*wordfish$wf_se

wordfish$wf_nosw_se <- wordfish$sd_wf_nosw/sqrt(wordfish$n_speeches)
wordfish$wf_nosw_ci_low <- wordfish$wf_nosw - 1.96*wordfish$wf_nosw_se
wordfish$wf_nosw_ci_up <- wordfish$wf_nosw + 1.96*wordfish$wf_nosw_se

wordfish$wf_notrim_se <- wordfish$sd_wf_notrim/sqrt(wordfish$n_speeches)
wordfish$wf_notrim_ci_low <- wordfish$wf_notrim - 1.96*wordfish$wf_notrim_se
wordfish$wf_notrim_ci_up <- wordfish$wf_notrim + 1.96*wordfish$wf_notrim_se


# merge
df <- merge(cosine_sim, wordfish, by = 'party')
df <- merge(smlse_1000, df, by = 'party')

partycols=c("blue", 'lightblue', "firebrick", "gold", "lightgreen", "black", "#FF6565")
df$party <- factor(df$party, levels = c("AfD", "independent", "PDS/LINKE", "FDP", "GRUENE", "CDU/CSU", "SPD"))


# plot per party
cols= brewer.pal(4, 'Set1')
df %>% ggplot() +
  facet_grid(rows = vars(party))+
  geom_point(aes(x=mean_est, y=0, col = cols[1])) +
  geom_linerange(aes(xmin = ci_low_est, xmax = ci_up_est, col = cols[1], y=0)) +
  geom_point(aes(x=mean_1000, y=-1, col = cols[2])) +
  geom_linerange(aes(xmin = ci_low_1000, xmax = ci_up_1000, col = cols[2], y=-1)) +
  geom_point(aes(x=-wf, y=-2, col = cols[3])) +
  geom_linerange(aes(xmin = -wf_ci_low, xmax = -wf_ci_up, col = cols[3], y=-2)) +
  geom_point(aes(x=mean_cs, y=-3, col = cols[4])) +
  geom_linerange(aes(xmin = ci_low_cs, xmax = ci_up_cs, col = cols[4], y=-3)) +
  geom_vline(xintercept = 0, col = 'red', lty = 2) +
  scale_y_continuous(name = ''
                     , breaks = c(), minor_breaks = c(0,-1,-2,-3)
                     # , breaks=c(0,-1,-2,-3), labels = c('SMLSE', 'SMLSE (subset)', 'Wordfish', 'Cosine Similarity')
                     ) + 
  scale_color_discrete(name = "", labels = c("SMLSE", "SMLSE (subset)", "Wordfish", "Cosine similarity"))+
  scale_x_continuous(name = 'Normalized estimate', limits = c(-1,3))


# plot per measure
p0 <- ggplot(df) + 
  geom_point(aes(x=mean_est, y=party, col = party)) +
  geom_linerange(aes(xmin = ci_low_est, xmax = ci_up_est, y=party, col = party)) +
  xlab('') + ylab('') + ggtitle('SMLSE') + scale_color_manual(values=partycols) +
  theme(legend.position = 'none') # + xlim(c(0,1))

p1 <- ggplot(df) + 
  geom_point(aes(x=mean_1000, y=party, col = party)) +
  geom_linerange(aes(xmin = ci_low_1000, xmax = ci_up_1000, y=party, col = party)) +
  xlab('') + ylab('') + ggtitle('SMLSE (restricted model)') + scale_color_manual(values=partycols) +
  theme(legend.position = 'none') # + xlim(c(0,1))

p2 <- ggplot(df) + 
  geom_point(aes(x=-wf, y=party, col = party)) +
  geom_linerange(aes(xmin = -wf_ci_low, xmax = -wf_ci_up, y=party, col = party)) +
  xlab('') + ylab('') + ggtitle('Wordfish') + scale_color_manual(values=partycols) +
  theme(legend.position = 'none')

p3 <- ggplot(df) + 
  geom_point(aes(x=mean_cs, y=party, col = party)) +
  geom_linerange(aes(xmin = ci_low_cs, xmax = ci_up_cs, y=party, col = party)) +
  xlab('') + ylab('') + ggtitle('Cosine similarity') + scale_color_manual(values=partycols) +
  theme(legend.position = 'none')


gridExtra::grid.arrange(p0,p1,p2,p3) %>% ggsave(filename = 'DE/vis/similarity_pts.jpg', height = 6, width = 8)
gridExtra::grid.arrange(p0,p1,p2,p3) %>% ggsave(filename = 'DE/vis/similarity_pts_presi.jpg', height = 5, width = 7)


