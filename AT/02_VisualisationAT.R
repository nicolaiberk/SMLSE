###########################################################
# MEASURING RHETORICAL SIMILARITY WITH SUPERVISED LEARNING
# VISUALISATION OF AUSTRIAN ESTIMATES
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

options(stringsAsFactors = FALSE)

# import fonts
font_import(pattern = '.*verdana.*', prompt = F)
loadfonts(device = "win", quiet = T)
windowsFonts(family = 'Verdana')

# import & clean data
setwd('AT')
at <- read.csv('smlse/AT_notext.csv', encoding = 'UTF-8')
at$date <- as.Date(at$date,format = '%Y-%m-%d')
at <- at[!at$party %in% c('', 'independent'),]
partycols=c("lightblue", "blue", "green", 'yellow', 'magenta', 'black', 'red')


## aggregate per month and party 
at$my <- floor_date(at$date, "quarterly")

at_pt_my <- at %>% 
  group_by(party, my) %>% 
  summarise(mean_rr = wtd.mean(x=RR_pred, w=n_words),
            sd_rr = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            mean_vp = wtd.mean(x=ÖVP_pred, w=n_words),
            sd_vp = sqrt(wtd.var(x=ÖVP_pred, weights = n_words)),
            mean_sp = wtd.mean(x=SPÖ_pred, w=n_words),
            sd_sp = sqrt(wtd.var(x=SPÖ_pred, weights = n_words)),
            mean_fp = wtd.mean(x=FPÖ_pred, w=n_words),
            sd_fp = sqrt(wtd.var(x=FPÖ_pred, weights = n_words)),
            mean_bz = wtd.mean(x=BZÖ_pred, w=n_words),
            sd_bz = sqrt(wtd.var(x=BZÖ_pred, weights = n_words)),
            n_speeches = length(RR_pred))

at_pt_my$se_rr <- at_pt_my$sd_rr/sqrt(at_pt_my$n_speeches)
at_pt_my$ci_low_rr <- at_pt_my$mean_rr - qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_rr
at_pt_my$ci_up_rr <- at_pt_my$mean_rr + qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_rr

at_pt_my$se_vp <- at_pt_my$sd_vp/sqrt(at_pt_my$n_speeches)
at_pt_my$ci_low_vp <- at_pt_my$mean_vp - qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_vp
at_pt_my$ci_up_vp <- at_pt_my$mean_vp + qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_vp

at_pt_my$se_sp <- at_pt_my$sd_sp/sqrt(at_pt_my$n_speeches)
at_pt_my$ci_low_sp <- at_pt_my$mean_sp - qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_sp
at_pt_my$ci_up_sp <- at_pt_my$mean_sp + qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_sp

at_pt_my$se_fp <- at_pt_my$sd_fp/sqrt(at_pt_my$n_speeches)
at_pt_my$ci_low_fp <- at_pt_my$mean_fp - qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_fp
at_pt_my$ci_up_fp <- at_pt_my$mean_fp + qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_fp

at_pt_my$se_bz <- at_pt_my$sd_bz/sqrt(at_pt_my$n_speeches)
at_pt_my$ci_low_bz <- at_pt_my$mean_bz - qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_bz
at_pt_my$ci_up_bz <- at_pt_my$mean_bz + qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_bz


#### Plot figure 1 ####

## FPÖ
fp_dta <- at_pt_my %>% 
  filter(party %in% c('FPÖ'))

fp <- ggplot(fp_dta, aes(x = my))+ 
  annotate('rect',xmin=as.Date(x = '18.12.2017', format = '%d.%m.%Y'),xmax=as.Date(x = '28.05.2019', format = '%d.%m.%Y'),ymin=0,ymax=0.5, alpha=0.1, fill ='red') +
  annotate('rect',xmin=as.Date(x = '04.02.2000', format = '%d.%m.%Y'),xmax=as.Date(x = '11.01.2007', format = '%d.%m.%Y'),ymin=0,ymax=0.5, alpha=0.1, fill ='red') + 
  geom_line(aes(y = fp_dta$mean_vp, col = 'ÖVP', lty = 'ÖVP')) +
  geom_line(aes(y = fp_dta$mean_sp, col = 'SPÖ', lty = 'SPÖ')) +
  geom_ribbon(alpha=0.2, aes(ymin = fp_dta$ci_low_vp, ymax = fp_dta$ci_up_vp, fill = 'ÖVP', col = 'ÖVP')) +
  geom_ribbon(alpha=0.2, aes(ymin = fp_dta$ci_low_sp, ymax = fp_dta$ci_up_sp, fill = 'SPÖ', col = 'SPÖ')) +
  ylab('Similarity') + xlab('') +
  ggtitle('Quarterly similarity estimates of radical-right FPÖ to...', subtitle = 'Red areas indicate ÖVP-FPÖ coalition governments') +
  theme(text = element_text(family='Verdana')) +
  scale_color_manual(name = '', values = c('ÖVP' = 'black', 'SPÖ' = 'red')) +
  scale_linetype_manual(name = '', values = c('ÖVP' = 'solid', 'SPÖ' = 'dashed')) +
  scale_fill_manual(name = '', values = c('ÖVP' = 'black', 'SPÖ' = 'red'))

ggsave('vis/AT_fp_poster.png', fp, height = 3, width = 15)
ggsave('vis/AT_fp_presi.png', fp, height = 4.5, width = 7.5)
ggsave('vis/AT_fp_paper.png', fp, height = 3, width = 7.5)


## ÖVP
vp_dta <- at_pt_my %>% 
  filter(party %in% c('ÖVP'))
  
vp <- ggplot(vp_dta, aes(x = my))+ 
  annotate('rect',xmin=as.Date(x = '18.12.2017', format = '%d.%m.%Y'),xmax=as.Date(x = '28.05.2019', format = '%d.%m.%Y'),ymin=0,ymax=0.5, alpha=0.1, fill ='red') +
  annotate('rect',xmin=as.Date(x = '04.02.2000', format = '%d.%m.%Y'),xmax=as.Date(x = '11.01.2007', format = '%d.%m.%Y'),ymin=0,ymax=0.5, alpha=0.1, fill ='red') + 
  geom_line(aes(y = vp_dta$mean_rr, col = 'FPÖ/BZÖ', lty = 'FPÖ/BZÖ')) +
  geom_line(aes(y = vp_dta$mean_sp, col = 'SPÖ', lty = 'SPÖ')) +
  geom_ribbon(alpha=0.2, aes(ymin = vp_dta$ci_low_rr, ymax = vp_dta$ci_up_rr, fill = 'FPÖ/BZÖ', col = 'FPÖ/BZÖ')) +
  geom_ribbon(alpha=0.2, aes(ymin = vp_dta$ci_low_sp, ymax = vp_dta$ci_up_sp, fill = 'SPÖ', col = 'SPÖ')) +
  ylab('Similarity') + xlab('') +
  ggtitle('Quarterly similarity estimates of centre-right ÖVP to...', subtitle = 'Red areas indicate ÖVP-FPÖ coalition governments') +
  theme(text = element_text(family='Verdana')) +
  scale_color_manual(name = '', values = c('FPÖ/BZÖ' = 'blue', 'SPÖ' = 'red')) +
  scale_linetype_manual(name = '', values = c('FPÖ/BZÖ' = 'solid', 'SPÖ' = 'dashed')) +
  scale_fill_manual(name = '', values = c('FPÖ/BZÖ' = 'blue', 'SPÖ' = 'red'))

ggsave('vis/AT_vp_poster.png', vp, height = 3, width = 15)
ggsave('vis/AT_vp_presi.png', vp, height = 4.5, width = 7.5)
ggsave('vis/AT_vp_paper.png', vp, height = 3, width = 7.5)



## aggregate per party
at_pt <- at %>% 
  group_by(party) %>% 
  summarise(mean_rr = wtd.mean(x=RR_pred, w=n_words),
            sd_rr = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            n_speeches = length(RR_pred))
at_pt$se <- at_pt$sd_rr/sqrt(at_pt$n_speeches)
at_pt$ci_low <- at_pt$mean_rr - qt(1 - (0.05 / 2), at_pt$n_speeches-1)*at_pt$se
at_pt$ci_up <- at_pt$mean_rr + qt(1 - (0.05 / 2), at_pt$n_speeches-1)*at_pt$se

# plot
party_plot <- ggplot(at_pt, aes(x=mean_rr, y=party, xmin = ci_low, xmax = ci_up, col = party)) + 
  geom_point(show.legend = F, size = 2) + 
  geom_linerange(size = 1, show.legend = F) +
  scale_color_manual(values=partycols)+
  xlab('SMLSE')
ggsave('vis/AT_parties_mean.png', party_plot, width = 6, height=3)


# plot density of party communication
party_dens_plot <- ggplot(at, aes(x=RR_pred, y=party, fill = party)) + 
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=at_pt, aes(x = mean_rr, y=party), show.legend = F)+
  # geom_linerange(data=at_pt, aes(xmin = ci_low, xmax = ci_up, y=party), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  xlab('SMLSE') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/AT_parties_density.png', party_dens_plot, width = 6, height=3)



#### Appendix ####

# B1: estimates including BZÖ
fpvpbz <- at_pt_my %>% 
  filter(party %in% c('FPÖ', 'ÖVP', 'BZÖ')) %>% 
  ggplot(aes(x = my, y = mean_rr, col=party, fill=party, ymin = ci_low_rr, ymax = ci_up_rr))+ 
  annotate('rect',xmin=as.Date(x = '18.12.2017', format = '%d.%m.%Y'),xmax=as.Date(x = '28.05.2019', format = '%d.%m.%Y'),ymin=0,ymax=1, alpha=0.1, fill ='red') +
  annotate('rect',xmin=as.Date(x = '04.02.2000', format = '%d.%m.%Y'),xmax=as.Date(x = '11.01.2007', format = '%d.%m.%Y'),ymin=0,ymax=1, alpha=0.1, fill ='red') + 
  geom_line() +
  geom_ribbon(alpha=0.2) +
  scale_color_manual(values=c('orange', 'blue', 'black')) +
  scale_fill_manual(values=c('orange', 'blue', 'black'))+
  ylab('SMLSE') +
  ggtitle('Austria') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/AT_fpvpbz_paper.png', fpvpbz, height = 3, width = 7.5)

# B2: estimates for FP only

## aggregate per month and party 
at_pt_my_fp <- at %>% 
  group_by(party, my) %>% 
  summarise(mean_rr = wtd.mean(x=FPÖ_pred, w=n_words),
            sd_rr = sqrt(wtd.var(x=FPÖ_pred, weights = n_words)),
            n_speeches = length(FPÖ_pred))

at_pt_my_fp$se <- at_pt_my_fp$sd_rr/sqrt(at_pt_my_fp$n_speeches)
at_pt_my_fp$ci_low <- at_pt_my_fp$mean_rr - qt(1 - (0.05 / 2), at_pt_my_fp$n_speeches-1)*at_pt_my_fp$se
at_pt_my_fp$ci_up <- at_pt_my_fp$mean_rr + qt(1 - (0.05 / 2), at_pt_my_fp$n_speeches-1)*at_pt_my_fp$se

## plot
fpvp_fp <- at_pt_my_fp %>% 
  filter(party %in% c('FPÖ', 'ÖVP')) %>% 
  ggplot(aes(x = my, y = mean_rr, col=party, fill=party, ymin = ci_low, ymax = ci_up))+ 
  annotate('rect',xmin=as.Date(x = '18.12.2017', format = '%d.%m.%Y'),xmax=as.Date(x = '28.05.2019', format = '%d.%m.%Y'),ymin=0,ymax=1, alpha=0.1, fill ='red') +
  annotate('rect',xmin=as.Date(x = '04.02.2000', format = '%d.%m.%Y'),xmax=as.Date(x = '11.01.2007', format = '%d.%m.%Y'),ymin=0,ymax=1, alpha=0.1, fill ='red') + 
  geom_line() +
  geom_ribbon(alpha=0.2) +
  scale_color_manual(values=c('blue', 'black')) +
  scale_fill_manual(values=c('blue', 'black'))+
  ylab('SMLSE') +
  ggtitle('Austria') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/AT_fpvp_fpest_paper.png', fpvp_fp, height = 4, width = 10)


# B3: VP estimates

## aggregate per month and party 
at_pt_my <- at %>% 
  group_by(party, my) %>% 
  summarise(mean_vp = wtd.mean(x=ÖVP_pred, w=n_words),
            sd_vp = sqrt(wtd.var(x=ÖVP_pred, weights = n_words)),
            n_speeches = length(ÖVP_pred))

at_pt_my$se <- at_pt_my$sd_vp/sqrt(at_pt_my$n_speeches)
at_pt_my$ci_low <- at_pt_my$mean_vp - qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se
at_pt_my$ci_up <- at_pt_my$mean_vp + qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se


# plot
vp_est <- at_pt_my %>% 
  filter(party %in% c('FPÖ', 'ÖVP')) %>% 
  ggplot(aes(x = my, y = mean_vp, col=party, fill=party, ymin = ci_low, ymax = ci_up))+ 
  annotate('rect',xmin=as.Date(x = '18.12.2017', format = '%d.%m.%Y'),xmax=as.Date(x = '28.05.2019', format = '%d.%m.%Y'),ymin=0,ymax=1, alpha=0.1, fill ='red') +
  annotate('rect',xmin=as.Date(x = '04.02.2000', format = '%d.%m.%Y'),xmax=as.Date(x = '11.01.2007', format = '%d.%m.%Y'),ymin=0,ymax=1, alpha=0.1, fill ='red') + 
  geom_line() +
  geom_ribbon(alpha=0.2) +
  scale_color_manual(values=c('blue', 'black')) +
  scale_fill_manual(values=c('blue', 'black'))+
  ylab('SMLSE') +
  ggtitle('Similarity estimate ÖVP', subtitle = 'monthly average') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/AT_vp_est_poster.png', vp_est, height = 3, width = 15)
ggsave('vis/AT_vp_est_presi.png', vp_est, height = 4.5, width = 7.5)
ggsave('vis/AT_vp_est_paper.png', vp_est, height = 3, width = 7.5)


# B4: increased sim VP mit FP 2018
at$month <- floor_date(at$date, "month")


# define dates
kurz_takeover <- as.Date('14.05.2017', format = '%d.%m.%Y')
# kurz_elected  <- as.Date('1.7.2017' ,format = '%d.%m.%Y')
election <- as.Date('15.10.2017' ,format = '%d.%m.%Y')
coalition <- as.Date('15.12.2017' ,format = '%d.%m.%Y')

at_pt_my <- at %>% 
  group_by(party, month) %>% 
  summarise(mean_rr = wtd.mean(x=RR_pred, w=n_words),
            sd_rr = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            mean_vp = wtd.mean(x=ÖVP_pred, w=n_words),
            sd_vp = sqrt(wtd.var(x=ÖVP_pred, weights = n_words)),
            n_speeches = length(RR_pred))

at_pt_my$se_rr <- at_pt_my$sd_rr/sqrt(at_pt_my$n_speeches)
at_pt_my$ci_low_rr <- at_pt_my$mean_rr - qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_rr
at_pt_my$ci_up_rr <- at_pt_my$mean_rr + qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_rr

at_pt_my$se_vp <- at_pt_my$sd_vp/sqrt(at_pt_my$n_speeches)
at_pt_my$ci_low_vp <- at_pt_my$mean_vp - qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_vp
at_pt_my$ci_up_vp <- at_pt_my$mean_vp + qt(1 - (0.05 / 2), at_pt_my$n_speeches-1)*at_pt_my$se_vp

kurz_vp <- at_pt_my %>% 
  filter(party %in% c('ÖVP', 'SPÖ')) %>% 
  ggplot(aes(x = month, y = mean_rr, col=party, fill=party, ymin = ci_low_rr, ymax = ci_up_rr))+ 
  geom_line() +
  geom_ribbon(alpha=0.2) +
  geom_vline(xintercept = c(kurz_takeover, election, coalition)) +
  geom_label(inherit.aes = F,
             label = 'Takeover\nKurz',
             aes(x =kurz_takeover, y = 0.25)) +
  geom_label(inherit.aes = F,
             label = 'Election',
             aes(x =election, y = 0.325)) +
  geom_label(inherit.aes = F,
             label = 'Coalition\nFormation',
             aes(x =coalition, y = 0.45)) +
  scale_color_manual(values=c('black', 'red')) +
  scale_fill_manual(values=c('black', 'red')) +
  ylab('SMLSE') +
  coord_cartesian(ylim = c(0,0.5)) +
  xlim(c(as.Date(x = '2016-12-31'), as.Date(x = '2018-07-01'))) +
  ggtitle('Development of similarity to FPÖ', subtitle = 'Monthly average, 1.1.2017-1.7.2018') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/AT_kurz_fp_est_presi.png', kurz_vp, height = 4.5, width = 7.5)
ggsave('vis/AT_kurz_fp_est_paper.png', kurz_vp, height = 3, width = 7.5)




# B5: increase sim FP mit VP 2018

kurz_fp <- at_pt_my %>% 
  filter(party %in% c('FPÖ', 'SPÖ')) %>% 
  ggplot(aes(x = month, y = mean_vp, col=party, fill=party, ymin = ci_low_vp, ymax = ci_up_vp))+ 
  geom_line() +
  geom_ribbon(alpha=0.2) +
  # geom_point(aes(x=at$date, y=at$RR_pred), alpha = 0.1, inherit.aes = F) +
  geom_vline(xintercept = c(kurz_takeover, election, coalition)) +
  geom_label(inherit.aes = F,
            label = 'Takeover\nKurz',
            aes(x =kurz_takeover, y = 0.25)) +
  geom_label(inherit.aes = F,
            label = 'Election',
            aes(x =election, y = 0.05)) +
  geom_label(inherit.aes = F,
            label = 'Coalition\nFormation',
            aes(x =coalition, y = 0.45), 
            nudge_x = 15) +
  scale_color_manual(values=c('blue', 'red')) +
  scale_fill_manual(values=c('blue', 'red')) +
  ylab('SMLSE') +
  coord_cartesian(ylim = c(0,0.5)) +
  xlim(c(as.Date(x = '2016-12-31'), as.Date(x = '2018-07-01'))) +
  ggtitle('Development of similarity to ÖVP', subtitle = 'Monthly average, 1.1.2017-1.7.2018') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/AT_kurz_vp_est_presi.png', kurz_fp, height = 4.5, width = 7.5)
ggsave('vis/AT_kurz_vp_est_paper.png', kurz_fp, height = 3, width = 7.5)

kurz_comp <- gridExtra::grid.arrange(kurz_vp, kurz_fp)
ggsave('vis/AT_kurz_comp.png', kurz_comp, height = 6, width = 7.5)


# B6: SP only, compaerd to either party
kurz_sp <- at_pt_my %>% 
  filter(party %in% c('SPÖ')) %>% 
  ggplot(aes(x = month, y = mean_vp, col=party, fill=party, ymin = ci_low_vp, ymax = ci_up_vp))+ 
  geom_line(col = 'black') +
  geom_line(aes(y=mean_rr), col = 'blue') +
  geom_ribbon(alpha=0.2, fill = 'black', col = 'black') +
  geom_ribbon(aes(y=mean_rr, ymin = ci_low_rr, ymax = ci_up_rr), alpha=0.2, fill = 'blue', col = 'blue') +
  # geom_point(aes(x=at$date, y=at$RR_pred), alpha = 0.1, inherit.aes = F) +
  geom_vline(xintercept = c(kurz_takeover, election, coalition)) +
  geom_label(inherit.aes = F,
             label = 'Takeover\nKurz',
             aes(x =kurz_takeover, y = 0.25)) +
  geom_label(inherit.aes = F,
             label = 'Election',
             aes(x =election, y = 0.05)) +
  geom_label(inherit.aes = F,
             label = 'Coalition\nFormation',
             aes(x =coalition, y = 0.45)) +
  ylab('SMLSE') +
  coord_cartesian(ylim = c(0,0.5)) +
  xlim(c(as.Date(x = '2016-12-31'), as.Date(x = '2018-07-01'))) +
  ggtitle('Development of SPÖ similarity', subtitle = 'Monthly average, 1.1.2017-1.7.2018') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/AT_kurz_sp.png', kurz_sp, height = 3, width = 7.5)
