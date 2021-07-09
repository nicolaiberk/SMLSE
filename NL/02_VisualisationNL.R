###########################################################
# MEASURING RHETORICAL SIMILARITY WITH SUPERVISED LEARNING
# VISUALISATION OF DUTCH ESTIMATES (Figure 3, App. B)
# Author: Nicolai Berk
###########################################################

# Setup
library(dplyr)
library(ggplot2)
library(lubridate)
library(boot)
library(Hmisc)
library(ggridges)
library(extrafont)


# import fonts
font_import(pattern = '.*verdana.*', prompt = F)
loadfonts(device = "win", quiet = T)
windowsFonts(family = 'Verdana')

# import and prepare data
setwd('NL')
options(stringsAsFactors = FALSE)
nl <- read.csv('smlse/NL_notext.csv', encoding = 'UTF-8')
nl$date <- as.Date(nl$date,format = '%Y-%m-%d')
nl <- nl %>% filter(date >= min(nl$date[party=='LPF'])) # restrict to time with RR parties
nl$familyname <- factor(nl$family, 
                    levels = c("RR", "OP", "CD", "LIB", "SD", "Left"),
                    labels = c("Radical Right", "Orthodox Protestant", "Christian Democrat", "Liberal", "Social Democrat", "Left"))
nl$family <- factor(nl$family, levels = c("RR", "OP", "CD", "LIB", "SD", "Left"))

partycols = c('blue', 'orange', 'green', 'lightblue', 'red', "firebrick")

nl$wk <- floor_date(nl$date, "week")
nl$my <- floor_date(nl$date, "month")
nl$qt <- floor_date(nl$date, "quarter")


#### define function for ci's#####
lower_ci <- function(var){
  
  Mean <- mean(var)
  SD <- sd(var)
  n_o <- length(var) - sum(is.na(var))
  se <- SD/sqrt(n_o)
  
  lower <- Mean - se*qt(1 - (0.05 / 2), n_o - 1)
  
  return(lower)
}

upper_ci <- function(var){
  
  Mean <- mean(var)
  SD <- sd(var)
  n_o <- length(var) - sum(is.na(var))
  se <- SD/sqrt(n_o)
  
  upper <- Mean + se*qt(1 - (0.05 / 2), n_o - 1)
  
  return(upper)
}




## calculate CI's per party and month
nl_sum <- list()
for (pt in unique(nl$party)){

  nl_sum[[pt]] <- nl %>% 
    filter(party == pt) %>% 
    group_by(qt) %>% 
    summarise(across(contains("pred"), 
                 list(lower = lower_ci, 
                      Mean = mean, 
                      upper = upper_ci)))
}


#### Appendix B Figure 1 ####

# vvd sim to PvdA, RR & CDA
vvd_sim <- 
  ggplot()+
  geom_line(data = nl_sum[["VVD"]], aes(x = qt, y = RR_pred_Mean, col = 'RR'  , lty = "RR")) +
  geom_line(data = nl_sum[["VVD"]], aes(x = qt, y = SD_pred_Mean, col = 'PvdA', lty = "PvdA")) +
  geom_line(data = nl_sum[["VVD"]], aes(x = qt, y = CDA_pred_Mean, col = 'CDA', lty = "CDA")) +
  geom_ribbon(data = nl_sum[["VVD"]], alpha=0.2, aes(x = qt, ymin = RR_pred_lower, ymax = RR_pred_upper, col = 'RR'), fill = "blue") +
  geom_ribbon(data = nl_sum[["VVD"]], alpha=0.2, aes(x = qt, ymin = SD_pred_lower, ymax = SD_pred_upper, col = 'PvdA'), fill = "red") +
  geom_ribbon(data = nl_sum[["VVD"]], alpha=0.2, aes(x = qt, ymin = CDA_pred_lower, ymax = CDA_pred_upper, col = 'CDA'), fill = "#007c5f") +
  geom_rect(xmin = as.Date('22.06.2002', format = '%d.%m.%Y'), xmax = as.Date('22.11.2006', format = '%d.%m.%Y'), ymin=.42, ymax=.43, aes(fill = 'w. CDA' )) +
  geom_rect(xmin = as.Date('22.06.2002', format = '%d.%m.%Y'), xmax = as.Date('16.10.2002', format = '%d.%m.%Y'), ymin=.40, ymax=.41, aes(fill = 'w. RR'  )) + # coalition breakdown 16.10.2002
  geom_rect(xmin = as.Date('14.10.2010', format = '%d.%m.%Y'), xmax = as.Date('23.04.2012', format = '%d.%m.%Y'), ymin=.42, ymax=.43, aes(fill = 'w. CDA' )) +
  geom_rect(xmin = as.Date('14.10.2010', format = '%d.%m.%Y'), xmax = as.Date('23.04.2012', format = '%d.%m.%Y'), ymin=.40, ymax=.41, aes(fill = 'w. RR'  )) +
  geom_rect(xmin = as.Date('05.11.2012', format = '%d.%m.%Y'), xmax = as.Date('14.03.2017', format = '%d.%m.%Y'), ymin=.41, ymax=.42, aes(fill = 'w. PvdA')) +
  geom_rect(xmin = as.Date('26.10.2017', format = '%d.%m.%Y'), xmax = max(nl_sum[["VVD"]]$qt),                    ymin=.42, ymax=.43, aes(fill = 'w. CDA' )) +
  ylab('Similarity') + xlab('') +
  ggtitle('Quarterly similarity estimates of liberal VVD', 
          subtitle = 'Top bars indicate governing status') +
  theme_minimal() +
  theme(text = element_text(family='Verdana'),
        legend.position = "right") +
  scale_color_manual(name = 'Similarity to:',
                     values = c("CDA" = "#007c5f", 
                                'PvdA' = 'red', 
                                'RR' = 'blue'), 
                     breaks = c("CDA", 
                                "PvdA", 
                                "RR")) +
  scale_linetype_manual(name = 'Similarity to:', 
                        values = c('RR' = 'dotted', 
                                   'PvdA' = 'dashed', 
                                   "CDA" = "solid"), 
                        breaks = c("CDA", 
                                   "PvdA", 
                                   "RR")) +
  scale_fill_manual(name = 'Governments:', 
                    values = c("w. CDA" = "#007c5f", 
                               "w. PvdA" = "red", 
                               "w. RR" = "blue"), 
                    breaks = c("w. CDA", 
                               "w. PvdA", 
                               "w. RR")) +
  guides(fill = guide_legend(order = 1, ), 
         col = guide_legend(order = 2, 
                            override.aes = list(fill = c("#007c5f", "red", "blue"))), 
         linetype = guide_legend(order = 2)) +
  
  coord_cartesian(ylim = c(0.1,0.42))

ggsave("vis/NL_vvd_sim.png",vvd_sim, height = 4.5, width = 7.5)


# CDA sim to PvdA, RR & VVD
cda_sim <- 
  ggplot()+
  geom_line(data = nl_sum[["CDA"]], aes(x = qt, y = VVD_pred_Mean, col = 'VVD', lty = "VVD")) +
  geom_line(data = nl_sum[["CDA"]], aes(x = qt, y = RR_pred_Mean, col = 'RR'  , lty = "RR")) +
  geom_line(data = nl_sum[["CDA"]], aes(x = qt, y = SD_pred_Mean, col = 'PvdA', lty = "PvdA")) +
  geom_ribbon(data = nl_sum[["CDA"]], alpha=0.2, aes(x = qt, ymin = SD_pred_lower, ymax = SD_pred_upper, col = 'PvdA'), fill = "red") +
  geom_ribbon(data = nl_sum[["CDA"]], alpha=0.2, aes(x = qt, ymin = RR_pred_lower, ymax = RR_pred_upper, col = 'RR'), fill = "blue") +
  geom_ribbon(data = nl_sum[["CDA"]], alpha=0.2, aes(x = qt, ymin = VVD_pred_lower, ymax = VVD_pred_upper, col = 'VVD'), fill = "#ff6400") +
  geom_rect(xmin = as.Date('22.06.2002', format = '%d.%m.%Y'), xmax = as.Date('22.11.2006', format = '%d.%m.%Y'), ymin=.44, ymax=.45, aes(fill = 'w. VVD' )) +
  geom_rect(xmin = as.Date('22.06.2002', format = '%d.%m.%Y'), xmax = as.Date('16.10.2002', format = '%d.%m.%Y'), ymin=.42, ymax=.43, aes(fill = 'w. RR'  )) + # coalition breakdown 16.10.2002
  geom_rect(xmin = as.Date('22.02.2007', format = '%d.%m.%Y'), xmax = as.Date('20.02.2010', format = '%d.%m.%Y'), ymin=.43, ymax=.44, aes(fill = 'w. PvdA')) +
  geom_rect(xmin = as.Date('14.10.2010', format = '%d.%m.%Y'), xmax = as.Date('23.10.2012', format = '%d.%m.%Y'), ymin=.44, ymax=.45, aes(fill = 'w. VVD' )) +
  geom_rect(xmin = as.Date('14.10.2010', format = '%d.%m.%Y'), xmax = as.Date('05.11.2012', format = '%d.%m.%Y'), ymin=.42, ymax=.43, aes(fill = 'w. RR'  )) +
  geom_rect(xmin = as.Date('26.10.2017', format = '%d.%m.%Y'), xmax = max(nl_sum[["VVD"]]$qt),                    ymin=.44, ymax=.45, aes(fill = 'w. VVD' )) +
  ylab('Similarity') + xlab('') +
  ggtitle('Quarterly similarity estimates of christian-democrat CDA', 
          subtitle = 'Top bars indicate governing status') +
  theme_minimal() +
  theme(text = element_text(family='Verdana'),
        legend.position = "right") +
  scale_color_manual(name = 'Similarity to:',
                     values = c("VVD" = "#ff6400", 
                                'PvdA' = 'red', 
                                'RR' = 'blue'), 
                     breaks = c("VVD", 
                                "PvdA", 
                                "RR")) +
  scale_linetype_manual(name = 'Similarity to:', 
                        values = c('RR' = 'dotted', 
                                   'PvdA' = 'dashed', 
                                   "VVD" = "solid"), 
                        breaks = c("VVD", 
                                   "PvdA", 
                                   "RR")) +
  scale_fill_manual(name = 'Governments:', 
                    values = c("w. VVD" = "#ff6400", 
                               "w. PvdA" = "red", 
                               "w. RR" = "blue"), 
                    breaks = c("w. VVD", 
                               "w. PvdA", 
                               "w. RR")) +
  guides(fill = guide_legend(order = 1, ), 
         col = guide_legend(order = 2, 
                            override.aes = list(fill = c("#ff6400", "red", "blue"))), 
         linetype = guide_legend(order = 2)) +
  coord_cartesian(ylim = c(0.1,0.44))

ggsave("vis/NL_cda_sim.png", cda_sim, height = 4.5, width = 7.5)



#### LPF sim to VVD & CDA
LPF_sim <- 
  ggplot()+ 
  annotate("rect", xmin = as.Date('22.06.2002', format = '%d.%m.%Y'), xmax = as.Date('16.10.2002', format = '%d.%m.%Y'), ymin=.000, ymax=1.00, fill = 'red', alpha = 0.2) +
  geom_line(data = nl_sum[["LPF"]], aes(x = qt, y = nl_sum[["LPF"]]$VVD_pred_Mean, col = 'VVD', lty = 'VVD')) +
  geom_line(data = nl_sum[["LPF"]], aes(x = qt, y = nl_sum[["LPF"]]$CDA_pred_Mean, col = 'CDA', lty = 'CDA')) +
  geom_ribbon(data = nl_sum[["LPF"]], aes(x = qt, ymin = nl_sum[["LPF"]]$VVD_pred_lower, ymax = nl_sum[["LPF"]]$VVD_pred_upper, col = 'VVD', fill = 'VVD'), alpha = 0.2) +
  geom_ribbon(data = nl_sum[["LPF"]], aes(x = qt, ymin = nl_sum[["LPF"]]$CDA_pred_lower, ymax = nl_sum[["LPF"]]$CDA_pred_upper, col = 'CDA', fill = 'CDA'), alpha = 0.2) +
  ylab('Similarity') + xlab('') +
  ggtitle('Quarterly similarity estimates of radical-right LPF', 
          subtitle = 'Red area indicates coalition government with CDA and VVD') +
  theme_minimal() +
  theme(text = element_text(family='Verdana')) +
  scale_color_manual(name = '', values = c('VVD' = '#ff6400', "CDA" = "#007c5f")) +
  scale_linetype_manual(name = '', values = c('VVD' = 'solid', "CDA" = "dashed")) +
  scale_fill_manual(name = '', values = c('VVD' = '#ff6400', "CDA" = "#007c5f")) +
  coord_cartesian(ylim = c(0.1,0.35))

ggsave("vis/NL_lpf_sim.png", LPF_sim, height = 3, width = 7.5)


#### PVV sim to VVD, CDA
PVV_sim <- 
  ggplot()+ 
  annotate('rect',xmin = as.Date('14.10.2010', format = '%d.%m.%Y'), xmax = as.Date('23.04.2012', format = '%d.%m.%Y'), ymin=0, ymax=1, alpha = 0.1, fill = 'red') +
  geom_line(data = nl_sum[["PVV"]], aes(x = qt, y = nl_sum[["PVV"]]$VVD_pred_Mean, col = 'VVD', lty = 'VVD')) +
  geom_line(data = nl_sum[["PVV"]], aes(x = qt, y = nl_sum[["PVV"]]$CDA_pred_Mean, col = 'CDA', lty = 'CDA')) +
  geom_ribbon(data = nl_sum[["PVV"]], alpha=0.2, aes(x = qt, ymin = nl_sum[["PVV"]]$VVD_pred_lower, ymax = nl_sum[["PVV"]]$VVD_pred_upper, fill = 'VVD', col = 'VVD')) +
  geom_ribbon(data = nl_sum[["PVV"]], alpha=0.2, aes(x = qt, ymin = nl_sum[["PVV"]]$CDA_pred_lower, ymax = nl_sum[["PVV"]]$CDA_pred_upper, fill = 'CDA', col = 'CDA')) +
  ylab('Similarity') + xlab('') +
  ggtitle('Quarterly similarity estimates of radical-right PVV', 
          subtitle = 'Red area indicates support for coalition government of VVD & CDA') +
  theme_minimal() +
  theme(text = element_text(family='Verdana')) +
  scale_color_manual(name = '', values = c('VVD' = '#ff6400', "CDA" = "#007c5f")) +
  scale_linetype_manual(name = '', values = c('VVD' = 'solid', "CDA" = "dashed")) +
  scale_fill_manual(name = '', values = c('VVD' = '#ff6400', "CDA" = "#007c5f")) +
  coord_cartesian(ylim = c(0.1,0.35))

ggsave("vis/NL_pvv_sim.png", PVV_sim, height = 3, width = 7.5)




#### aggregate by party family per quarter ####
nl_pt <- nl %>% 
  group_by(family, qt) %>% 
  summarise(mean_w = weighted.mean(x=RR_pred, w=n_words),
            sd_w = sqrt(wtd.var(x=RR_pred, weights=n_words)),
            n_speeches = length(RR_pred))
nl_pt$se <- nl_pt$sd_w/sqrt(nl_pt$n_speeches)
nl_pt$ci_low <- nl_pt$mean_w - qt(1 - (0.05 / 2), nl_pt$n_speeches - 1)*nl_pt$se
nl_pt$ci_up <- nl_pt$mean_w + qt(1 - (0.05 / 2), nl_pt$n_speeches - 1)*nl_pt$se

#plot 
nl_fam_qt <- nl_pt %>% 
  ggplot(aes(x=qt, y=mean_w, col=family, fill=family, ymin=ci_low, ymax=ci_up))+ 
  # geom_linerange(xmin=min(nl$date[nl$party=='LPF']),xmax=max(nl$date[nl$party=='LPF']),y=1.025, col ='yellow', size = 2, inherit.aes = F) +
  # geom_linerange(xmin=min(nl$date[nl$party=='PVV']),xmax=max(nl$date[nl$party=='PVV']),y=1.025, col ='blue', size = 2, inherit.aes = F) +
  # geom_linerange(xmin=min(nl$date[nl$party=='FvD']),xmax=max(nl$date[nl$party=='FvD']),y=1.0, col ='red', size = 2, inherit.aes = F) +
  annotate('rect',xmin = as.Date('22.06.2002', format = '%d.%m.%Y'), xmax = as.Date('16.10.2002', format = '%d.%m.%Y'), ymin=0, ymax=1, alpha = 0.1, fill = 'red') +
  annotate('rect',xmin = as.Date('14.10.2010', format = '%d.%m.%Y'), xmax = as.Date('05.11.2012', format = '%d.%m.%Y'), ymin=0, ymax=1, alpha = 0.1, fill = 'red') +
  # geom_vline(xintercept=as.Date('09.06.2010', format = '%d.%m.%Y'))+ # election 2010 - after pvv accomodated
  geom_ribbon(alpha = 0.2)+
  scale_fill_manual(values=partycols)+
  scale_color_manual(values=partycols)+
  geom_line()+
  xlab(label = '')+
  ylab('SMLSE') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/nl_families_monthly.png', nl_fam_qt)


#### aggregate by party per quarter ####
nl_pt <- nl %>% 
  filter(party %in% c('VVD', 'CDA') | familyname == 'Radical Right') %>% 
  group_by(party, qt) %>% 
  summarise(mean_w = weighted.mean(x=RR_pred, w=n_words),
            sd_w = sqrt(wtd.var(x=RR_pred, weights=n_words)),
            n_speeches = length(RR_pred))
nl_pt$se <- nl_pt$sd_w/sqrt(nl_pt$n_speeches)
nl_pt$ci_low <- nl_pt$mean_w - qt(1 - (0.05 / 2), nl_pt$n_speeches - 1)*nl_pt$se
nl_pt$ci_up <- nl_pt$mean_w + qt(1 - (0.05 / 2), nl_pt$n_speeches - 1)*nl_pt$se


#plot VVD only
partycols = c('gold', 'blue', 'orange', 'darkgreen', 'red')
nl_pt$party <- factor(nl_pt$party, levels = c("LPF", "PVV", "VVD", "CDA", "FvD"))

nl_vvd_qt <- nl_pt %>%
  filter(!party %in% c('CDA', 'FvD')) %>% 
  ggplot(aes(x=qt, y=mean_w, col=party, fill=party, ymin=ci_low, ymax=ci_up))+ 
  # geom_linerange(xmin=min(nl$date[nl$party=='LPF']),xmax=max(nl$date[nl$party=='LPF']),y=1.025, col ='yellow', size = 2, inherit.aes = F) +
  # geom_linerange(xmin=min(nl$date[nl$party=='PVV']),xmax=max(nl$date[nl$party=='PVV']),y=1.025, col ='blue', size = 2, inherit.aes = F) +
  # geom_linerange(xmin=min(nl$date[nl$party=='FvD']),xmax=max(nl$date[nl$party=='FvD']),y=1.0, col ='red', size = 2, inherit.aes = F) +
  annotate('rect',xmin = as.Date('22.06.2002', format = '%d.%m.%Y'), xmax = as.Date('16.10.2002', format = '%d.%m.%Y'), ymin=0, ymax=1, alpha = 0.1, fill = 'red') +
  annotate('rect',xmin = as.Date('14.10.2010', format = '%d.%m.%Y'), xmax = as.Date('05.11.2012', format = '%d.%m.%Y'), ymin=0, ymax=1, alpha = 0.1, fill = 'red') +
  # geom_vline(xintercept=as.Date('09.06.2010', format = '%d.%m.%Y'))+ # election 2010 - after pvv accomodated
  geom_line()+
  geom_ribbon(alpha = 0.2)+
  scale_fill_manual(values=partycols)+
  scale_color_manual(values=partycols)+
  xlab(label = '')+
  ylab('SMLSE')+
  ggtitle('The Netherlands') +
  theme_minimal() +
  theme(text = element_text(family='Verdana'))

ggsave('vis/nl_vvd_rr_poster.png', nl_vvd_qt, height = 4, width = 20)
ggsave('vis/nl_vvd_rr_presi.png', nl_vvd_qt, height = 6, width = 10)
ggsave('vis/nl_vvd_rr_paper.png', nl_vvd_qt, height = 4, width = 10)


# including CDA
nl_full_qt <- nl_pt %>%
  ggplot(aes(x=qt, y=mean_w, col=party, fill=party, ymin=ci_low, ymax=ci_up))+ 
  # geom_linerange(xmin=min(nl$date[nl$party=='LPF']),xmax=max(nl$date[nl$party=='LPF']),y=1.025, col ='yellow', size = 2, inherit.aes = F) +
  # geom_linerange(xmin=min(nl$date[nl$party=='PVV']),xmax=max(nl$date[nl$party=='PVV']),y=1.025, col ='blue', size = 2, inherit.aes = F) +
  # geom_linerange(xmin=min(nl$date[nl$party=='FvD']),xmax=max(nl$date[nl$party=='FvD']),y=1.0, col ='red', size = 2, inherit.aes = F) +
  annotate('rect',xmin = as.Date('22.06.2002', format = '%d.%m.%Y'), xmax = as.Date('16.10.2002', format = '%d.%m.%Y'), ymin=0, ymax=1, alpha = 0.1, fill = 'red') +
  annotate('rect',xmin = as.Date('14.10.2010', format = '%d.%m.%Y'), xmax = as.Date('05.11.2012', format = '%d.%m.%Y'), ymin=0, ymax=1, alpha = 0.1, fill = 'red') +
  # geom_vline(xintercept=as.Date('09.06.2010', format = '%d.%m.%Y'))+ # election 2010 - after pvv accomodated
  geom_line()+
  geom_ribbon(alpha = 0.2)+
  scale_fill_manual(values=partycols)+
  scale_color_manual(values=partycols)+
  xlab(label = '')+
  ylab('SMLSE') +
  theme_minimal() +
  theme(text = element_text(family='Verdana'))

ggsave('vis/nl_vvd_cda_rr.png', nl_full_qt, height = 4, width = 10)



## aggregate per party family
nl_pt <- nl %>% 
  group_by(family) %>% 
  summarise(mean_w = wtd.mean(x=RR_pred, w=n_words),
            sd_w = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            n_speeches = length(RR_pred))
nl_pt$se <- nl_pt$sd_w/sqrt(nl_pt$n_speeches)
nl_pt$ci_low <- nl_pt$mean_w - qt(1 - (0.05 / 2), nl_pt$n_speeches - 1)*nl_pt$se
nl_pt$ci_up <- nl_pt$mean_w + qt(1 - (0.05 / 2), nl_pt$n_speeches - 1)*nl_pt$se

# plot
partycols = c('blue', 'orange', 'green', 'lightblue', 'red', "firebrick")
party_plot <- ggplot(nl_pt, aes(x=mean_w, y=family, xmin = ci_low, xmax = ci_up, col = family)) + 
  geom_point(show.legend = F, size = 2) + 
  geom_linerange(size = 1, show.legend = F) +
  scale_color_manual(values=partycols)+
  xlab('SMLSE') +
  theme_minimal() +
  theme(text = element_text(family='Verdana'))

ggsave('vis/nl_parties_point.png', party_plot, width = 6, height=3)


# plot density per family
party_dens_plot <- ggplot(nl, aes(x=RR_pred, y=family, fill = family)) + 
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=nl_pt, aes(x = mean_w, y=family), show.legend = F)+
  # geom_linerange(data=_pt, aes(xmin = ci_low, xmax = ci_up, y=family), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  xlab('SMLSE') +
  theme_minimal() +
  theme(text = element_text(family='Verdana'))

ggsave('vis/nl_parties_density.png', party_dens_plot, width = 8, height=5)





#### track wilders ####
nl_wilders <- nl[nl$speaker %in% c('Wilders', 'De heer Wilders', "Rutte", "Mark Rutte", "De heer Rutte", "De heer Mark Rutte"),]

nl_wilders$Name <- 'Rutte'
nl_wilders$Name[nl_wilders$speaker %in% c('Wilders', 'De heer Wilders')] <- 'Wilders'


### simple smoothing method for all speeches:
plot_smooth <- nl_wilders %>% 
  filter(date < as.Date("2005-01-01")) %>% 
  ggplot(aes(x=date, y = RR_pred, col = Name)) +
  geom_point(alpha = 0.1)+
  geom_smooth(method = "loess")+
  geom_vline(xintercept = as.Date('04.09.2004', format = '%d.%m.%Y'))+
  xlab('')+
  xlim(c(min(nl_wilders$date), as.Date('2005-01-01')))+
  ylab('Similarity to the LPF')+
  theme_minimal() +
  theme(legend.position="top", legend.title=element_blank()) +
  theme(text = element_text(family='Verdana')) +
  scale_color_manual(values = c('#00BFC4', '#FF9999')) +
  coord_cartesian(ylim = c(0,1))

ggsave('vis/Wilders_Rutte_smoothed.png', plot_smooth, width = 6, height = 4)



### aggregated to month:
wilders <- nl_wilders %>% group_by(my, Name) %>%
  summarise(mean = wtd.mean(x=RR_pred, w=n_words),
            sd = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            n_speeches = length(RR_pred),
            n_words = n_words)
wilders$se <- wilders$sd/sqrt(wilders$n_speeches)
wilders$ci_low <- wilders$mean - qt(1 - (0.05 / 2), wilders$n_speeches - 1)*wilders$se
wilders$ci_up <- wilders$mean + qt(1 - (0.05 / 2), wilders$n_speeches - 1)*wilders$se


plot <- 
  ggplot(wilders, aes(x=my, y=mean, ymin=ci_low, ymax=ci_up, col = Name))+
  geom_line(aes(lty = Name))+
  geom_ribbon(alpha=0.2)+
  # geom_rug(
  #   inherit.aes = F,
  #   sides = "b",
  #   aes(x = nl_wilders$date,
  #       col = Name
  #       # alpha = 0.1,
  #       # position = 'jitter'
  #   ))+
  geom_vline(xintercept = as.Date('01.09.2004', format = '%d.%m.%Y'))+
  xlab('')+
  xlim(c(min(wilders$my), as.Date('2005-01-01')))+
  ylab('Similarity to the LPF')+
  theme_minimal() +
  theme(legend.position="top", legend.title=element_blank()) +
  theme(text = element_text(family='Verdana')) +
  scale_color_manual(values = c('#00BFC4', '#FF9999')) +
  coord_cartesian(ylim = c(0,0.8))
  

ggsave('vis/Wilders_Rutte.png', plot, width = 5, height = 5)



## wilders compared to rest of VVD before 2. September 2004
vvd <- nl %>% 
  filter(party == 'VVD' & (date < as.Date('2004-09-04') & (date > as.Date('2004-01-01')))) %>% 
  group_by(speaker) %>%
  summarise(mean = wtd.mean(x=RR_pred, w=n_words),
            sd = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            n_speeches = length(RR_pred))
vvd$se <- vvd$sd/sqrt(vvd$n_speeches)
vvd$ci_low <- vvd$mean - qt(1 - (0.05 / 2), vvd$n_speeches - 1)*vvd$se
vvd$ci_up <- vvd$mean + qt(1 - (0.05 / 2), vvd$n_speeches - 1)*vvd$se

vvd$highlight <- 'a'
vvd$highlight[grep(vvd$speaker, pattern = 'Wilders')] <- 'b'
vvd$highlight[grep(vvd$speaker, pattern = 'Verdonk')] <- 'c'
vvd$highlight[grep(vvd$speaker, pattern = 'Ali')] <- 'c'
vvd$highlight[grep(vvd$speaker, pattern = 'Oplaat')] <- 'c'
vvd$highlight[grep(vvd$speaker, pattern = 'Rutte')] <- 'd'
vvd$highlight[grep(vvd$speaker, pattern = 'Schultz')] <- 'd'

vvd$speaker[grepl(vvd$speaker, pattern = 'Schultz')] <- 'Schultz'

plot2 <- ggplot(vvd, aes(x = mean, xmin = ci_low, xmax = ci_up, y= reorder(speaker, -mean), col = highlight))+
  geom_pointrange(show.legend = F, fatten = ((vvd$highlight != 'a')+1)*2)+
  scale_color_manual(values = c('gray', '#FF0000', '#FF9999', '#00BFC4'))+
  xlab('Similarity to the LPF')+
  ylab('') +
  theme_minimal() +
  theme(text = element_text(family='Verdana')) +
  coord_cartesian(xlim = c(0,0.35))

  # ggtitle("SMLSE for VVD members preceding Wilders' exit")

ggsave('vis/Wilders_comp.png', plot2, width = 4, height = 4)



# with fewer speaker names
vvd <- arrange(vvd, -mean)
vvd$speaker_alt <- ""
vvd$speaker_alt[vvd$speaker %in% c("Wilders", "Verdonk", "Hirsi Ali", 
                                   "Oplaat", "Rutte", "Schultz")] <- 
  vvd$speaker[vvd$speaker %in% c("Wilders", "Verdonk",  "Hirsi Ali",
                                 "Oplaat", "Rutte", "Schultz")]


plot2_r <- ggplot(vvd, aes(x = mean, xmin = ci_low, xmax = ci_up, 
                         y= reorder(speaker, -mean), 
                         col = highlight))+
  geom_pointrange(show.legend = F, fatten = ((vvd$highlight != 'a')+1)*2)+
  scale_y_discrete(labels = vvd$speaker_alt) +
  scale_color_manual(values = c('gray', '#FF0000', '#FF9999', '#00BFC4'))+
  xlab('Similarity to the LPF')+
  ylab('') +
  theme_minimal() +
  theme(text = element_text(family='Verdana')) +
  coord_cartesian(xlim = c(0,0.35))

ggsave('vis/Wilders_comp_r.png', plot2_r, width = 4, height = 4)
ggsave('vis/Wilders_comp_r_pres.png', plot2_r, width = 6, height = 4)
gridExtra::grid.arrange(plot, plot2_r, nrow = 1) %>% 
  ggsave(filename = 'vis/Wilders_both_r.png', width = 8, height = 4)
gridExtra::grid.arrange(plot, plot2_r, nrow = 1) %>% 
  ggsave(filename = 'vis/Wilders_both.png', width = 12, height = 6)



