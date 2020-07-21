# vis NL - still need to update classifier to run

library(dplyr)
library(ggplot2)
library(lubridate)
library(boot)
library(Hmisc)
library(ggridges)

rm(list=ls())
setwd('C:/Users/nicol/Dropbox/Studium/Amsterdam/Studies/Semester 4/Master Thesis/data/dataverse_files')

options(stringsAsFactors = FALSE)

rm(list = ls())
nl <- read.csv('NL_notext20200627.csv', encoding = 'UTF-8')

nl$date <- as.Date(nl$date,format = '%Y-%m-%d')

nl <- nl %>% filter(date >= min(nl$date[party=='LPF'])) # restrict to time with RR parties

nl$family[nl$family=='RR'] <- 'Radical Right'
nl$family <- factor(nl$family, levels = c("Radical Right", "Orthodox Protestant", "Christian Democrat", "Liberal", "Social Democrat", "Left"))


partycols = c('blue', 'orange', 'green', 'lightblue', 'red', "firebrick")

#### aggregate by party family per month ####
nl$my <- floor_date(nl$date, "month")
nl_pt <- nl %>% 
  group_by(family, my) %>% 
  summarise(mean_w = weighted.mean(x=RR_pred, w=n_words),
            sd_w = sqrt(wtd.var(x=RR_pred, weights=n_words)),
            n_speeches = length(RR_pred))
nl_pt$se <- nl_pt$sd_w/sqrt(nl_pt$n_speeches)
nl_pt$ci_low <- nl_pt$mean_w - 1.96*nl_pt$se
nl_pt$ci_up <- nl_pt$mean_w + 1.96*nl_pt$se

#plot 
nl_fam_my <- nl_pt %>% 
  ggplot(aes(x=my, y=mean_w, col=family, fill=family, ymin=ci_low, ymax=ci_up))+ 
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
  ylab('SMLSE')

ggsave('vis/nl_fam_my.png', nl_fam_my)


#### aggregate by party per month ####
nl_pt <- nl %>% 
  filter(party %in% c('VVD', 'CDA') | family == 'Radical Right') %>% 
  group_by(party, my) %>% 
  summarise(mean_w = weighted.mean(x=RR_pred, w=n_words),
            sd_w = sqrt(wtd.var(x=RR_pred, weights=n_words)),
            n_speeches = length(RR_pred))
nl_pt$se <- nl_pt$sd_w/sqrt(nl_pt$n_speeches)
nl_pt$ci_low <- nl_pt$mean_w - 1.96*nl_pt$se
nl_pt$ci_up <- nl_pt$mean_w + 1.96*nl_pt$se


#plot VVD only
partycols = c('gold', 'blue', 'orange', 'darkgreen', 'red')
nl_pt$party <- factor(nl_pt$party, levels = c("LPF", "PVV", "VVD", "CDA", "FvD"))

nl_vvd_my <- nl_pt %>%
  filter(!party %in% c('CDA', 'FvD')) %>% 
  ggplot(aes(x=my, y=mean_w, col=party, fill=party, ymin=ci_low, ymax=ci_up))+ 
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
  ylab('SMLSE')

ggsave('vis/nl_vvd_my.png', nl_vvd_my)
ggsave('vis/nl_poster.png', nl_vvd_my, height = 4, width = 20)
ggsave('vis/nl_presi.png', nl_vvd_my, height = 6, width = 10)
ggsave('vis/nl_paper.png', nl_vvd_my, height = 4, width = 10)


# including CDA
nl_full_my <- nl_pt %>%
  ggplot(aes(x=my, y=mean_w, col=party, fill=party, ymin=ci_low, ymax=ci_up))+ 
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
  ylab('SMLSE')
ggsave('vis/nl_full.png', nl_full_my, height = 4, width = 10)



## aggregate per party family
nl_pt <- nl %>% 
  group_by(family) %>% 
  summarise(mean_w = wtd.mean(x=RR_pred, w=n_words),
            sd_w = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            n_speeches = length(RR_pred))
nl_pt$se <- nl_pt$sd_w/sqrt(nl_pt$n_speeches)
nl_pt$ci_low <- nl_pt$mean_w - 1.96*nl_pt$se
nl_pt$ci_up <- nl_pt$mean_w + 1.96*nl_pt$se

# plot
partycols = c('blue', 'orange', 'green', 'lightblue', 'red', "firebrick")
party_plot <- ggplot(nl_pt, aes(x=mean_w, y=family, xmin = ci_low, xmax = ci_up, col = family)) + 
  geom_point(show.legend = F, size = 2) + 
  geom_linerange(size = 1, show.legend = F) +
  scale_color_manual(values=partycols)+
  xlab('SMLSE')
ggsave('vis/nl_parties.png', party_plot, width = 6, height=3)


# plot density per family
party_dens_plot <- ggplot(nl, aes(x=RR_pred, y=family, fill = family)) + 
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=nl_pt, aes(x = mean_w, y=family), show.legend = F)+
  # geom_linerange(data=_pt, aes(xmin = ci_low, xmax = ci_up, y=family), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  xlab('SMLSE')
ggsave('vis/nl_parties_density.png', party_dens_plot, width = 6, height=3)



## track wilders
wilders <- nl[nl$speaker %in% c('Wilders', 'De heer Wilders', "Rutte", "Mark Rutte", "De heer Rutte", "De heer Mark Rutte"),]

wilders$Name <- 'Mark Rutte'
wilders$Name[wilders$speaker %in% c('Wilders', 'De heer Wilders')] <- 'Geert Wilders'

wilders$my <- floor_date(wilders$date, 'quarters')

# ggplot(wilders, aes(x=date, y=RR_pred, col = name, lty = name))+
#   geom_line()+
#   geom_vline(xintercept = as.Date('01.07.2004', format = '%d.%m.%Y'))

wilders <- wilders %>% group_by(my, Name) %>%
  summarise(mean = wtd.mean(x=RR_pred, w=n_words),
            sd = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            n_speeches = length(RR_pred),
            n_words = n_words)
wilders$se <- wilders$sd/sqrt(wilders$n_speeches)
wilders$ci_low <- wilders$mean - 1.96*wilders$se
wilders$ci_up <- wilders$mean + 1.96*wilders$se

plot <- 
  ggplot(wilders, aes(x=my, y=mean, ymin=ci_low, ymax=ci_up, col = Name))+
  geom_line()+
  geom_ribbon(alpha=0.2)+
  geom_vline(xintercept = as.Date('04.09.2004', format = '%d.%m.%Y'))+
  xlab('')+
  xlim(c(min(wilders$my), as.Date('2005-01-01')))+
  ylim(c(-0.10, 0.4))+
  ylab('SMLSE')+
  # ggtitle('Quarterly mean SMLSE for Geert Wilders and Mark Rutte')+ 
  theme(legend.position="bottom", legend.title=element_blank())
ggsave('vis/Wilders_Rutte.png', plot, width = 10, height = 4)


## wilders compared to rest of VVD before 2. September 2004
vvd <- nl %>% 
  filter(party == 'VVD' & (date < as.Date('2004-09-04') & (date > as.Date('2004-01-01')))) %>% 
  group_by(speaker) %>%
  summarise(mean = wtd.mean(x=RR_pred, w=n_words),
            sd = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            n_speeches = length(RR_pred))
vvd$se <- vvd$sd/sqrt(vvd$n_speeches)
vvd$ci_low <- vvd$mean - 1.96*vvd$se
vvd$ci_up <- vvd$mean + 1.96*vvd$se

vvd$highlight <- 'a'
vvd$highlight[grep(vvd$speaker, pattern = 'Wilders')] <- 'b'
vvd$highlight[grep(vvd$speaker, pattern = 'Verdonk')] <- 'c'
vvd$highlight[grep(vvd$speaker, pattern = 'Ali')] <- 'c'
vvd$highlight[grep(vvd$speaker, pattern = 'Oplaat')] <- 'c'
vvd$highlight[grep(vvd$speaker, pattern = 'Rutte')] <- 'd'
vvd$highlight[grep(vvd$speaker, pattern = 'Schultz')] <- 'd'

vvd$speaker[grepl(vvd$speaker, patter = 'Schultz')] <- 'Schultz'

plot2 <- ggplot(vvd, aes(x = mean, xmin = ci_low, xmax = ci_up, y= reorder(speaker, -mean), col = highlight))+
  geom_pointrange(show.legend = F)+
  scale_color_manual(values = c('gray', '#FF0000', '#FF9999', '#00BFC4'))+
  xlab('SMLSE')+
  ylab('')
  # ggtitle("SMLSE for VVD members preceding Wilders' exit")

ggsave('vis/Wilders_comp.png', plot2, width = 6, height = 6)


gridExtra::grid.arrange(plot, plot2, nrow = 1) %>% 
  ggsave(filename = 'vis/Wilders_both.png', width = 12, height = 6)


