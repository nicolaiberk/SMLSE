# vis AT

library(dplyr)
library(ggplot2)
library(lubridate)
library(boot)
library(Hmisc)
library(ggridges)

options(stringsAsFactors = FALSE)

at <- read.csv('smlse/AT_notext.csv', encoding = 'UTF-8')

at$date <- as.Date(at$date,format = '%Y-%m-%d')

colnames(at)[10] <- 'bzoe'
colnames(at)[11] <- 'fpoe'
colnames(at)[12] <- 'bzoe_pred'
colnames(at)[13] <- 'fpoe_pred'


at <- at[!at$party %in% c('', 'independent'),]
partycols=c("lightblue", "blue", "green", 'yellow', 'magenta', 'black', 'red')


## aggregate per month and party 
at$my <- floor_date(at$date, "month")

at_pt_my <- at %>% 
  group_by(party, my) %>% 
  summarise(mean_rr = wtd.mean(x=RR_pred, w=n_words),
            sd_rr = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            n_speeches = length(RR_pred))

at_pt_my$se <- at_pt_my$sd_rr/sqrt(at_pt_my$n_speeches)
at_pt_my$ci_low <- at_pt_my$mean_rr - 1.96*at_pt_my$se
at_pt_my$ci_up <- at_pt_my$mean_rr + 1.96*at_pt_my$se


# plot
fpvp <- at_pt_my %>% 
  filter(party %in% c('FPÖ', 'ÖVP')) %>% 
  ggplot(aes(x = my, y = mean_rr, col=party, fill=party, ymin = ci_low, ymax = ci_up))+ 
  annotate('rect',xmin=as.Date(x = '18.12.2017', format = '%d.%m.%Y'),xmax=as.Date(x = '28.05.2019', format = '%d.%m.%Y'),ymin=0,ymax=1, alpha=0.1, fill ='red') +
  annotate('rect',xmin=as.Date(x = '04.02.2000', format = '%d.%m.%Y'),xmax=as.Date(x = '11.01.2007', format = '%d.%m.%Y'),ymin=0,ymax=1, alpha=0.1, fill ='red') + 
  geom_line() +
  geom_ribbon(alpha=0.2) +
  scale_color_manual(values=c('blue', 'black')) +
  scale_fill_manual(values=c('blue', 'black'))+
  ylab('SMLSE')

ggsave('vis/AT_fpvp_poster.png', fpvp, height = 4, width = 20)
ggsave('vis/AT_fpvp_presi.png', fpvp, height = 6, width = 10)
ggsave('vis/AT_fpvp_paper.png', fpvp, height = 4, width = 10)


## aggregate per party
at_pt <- at %>% 
  group_by(party) %>% 
  summarise(mean_rr = wtd.mean(x=RR_pred, w=n_words),
            sd_rr = sqrt(wtd.var(x=RR_pred, weights = n_words)),
            n_speeches = length(RR_pred))
at_pt$se <- at_pt$sd_rr/sqrt(at_pt$n_speeches)
at_pt$ci_low <- at_pt$mean_rr - 1.96*at_pt$se
at_pt$ci_up <- at_pt$mean_rr + 1.96*at_pt$se

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
  xlab('SMLSE')
ggsave('vis/AT_parties_density.png', party_dens_plot, width = 6, height=3)

