# visualisation of predictors
library(dplyr)
library(ggplot2)
library(lubridate)
library(boot)
library(Hmisc)
library(ggridges)

options(stringsAsFactors = FALSE)

# Import ####
de <- read.csv('DE_notext_20200526.csv', fileEncoding = 'UTF-8')
de$date <- as.Date(de$date)

## vis #####
de <- de[!de$party %in% c(''),]

de$party <- factor(de$party, levels = c("AfD", "CDU/CSU", "SPD", "FDP", "GRUENE", "PDS/LINKE", "independent"))

partycols=c("lightblue", "grey", "#FF6565", "gold", "lightgreen", "firebrick", 'blue')



## aggregate per speaker
de_sp <- de %>% 
  group_by(speaker, party) %>% 
  summarise(mean_w = weighted.mean(x=afd_pred, w=n_words_raw),
            sd_w = sqrt(wtd.var(x=afd_pred, weights=n_words_raw, normwt = T)),
            n_speeches = length(afd_pred),
            n_words = sum(n_words_raw))
de_sp$se <- de_sp$sd_w/sqrt(de_sp$n_speeches)
de_sp$ci_low <- de_sp$mean_w - 1.96*de_sp$se
de_sp$ci_up <- de_sp$mean_w + 1.96*de_sp$se

# plot
afd_members <- c('Alice Weidel', 'Frauke Petry') 
cdu_members <- c('Angela Merkel', 'Christian Freiherr von Stetten') 
spd_members <- c('Martin Schulz','Andrea Nahles')
fdp_members <- c('Christian Lindner', 'Karlheinz Busen')
green_members <- c('Annalena Baerbock', 'Renate Künast')
left_members <- c('Fabio De Masi', 'Katja Kipping')
future_ind <- c('Lars Herrmann', 'Uwe Kamann', 'Verena Hartmann')
important_members <- c(afd_members, cdu_members, spd_members, fdp_members, green_members, left_members, future_ind)

de_sp$labels = ""
de_sp$labels[de_sp$speaker %in% important_members] <- de_sp$speaker[de_sp$speaker %in% important_members]
de_sp$labelcol = F
de_sp$labelcol[de_sp$speaker %in% important_members]<- T

de_sp$labels[de_sp$speaker %in% c('Lars Herrmann', 'Verena Hartmann')] <- ''

speaker_plot <-
  ggplot(de_sp[de_sp$n_words>100,],aes(x=party, y=mean_w, fill=party, label = labels, col = labelcol)) + 
  geom_dotplot(binwidth=0.015, method='histodot', drop = T, stackratio = .7, binaxis = 'y', stackdir = 'center', show.legend = F, stroke = 2.5)+
  geom_text(aes(label=labels), col='black', position = 'identity', size = 5, vjust=-1) +
  scale_fill_manual(values=partycols) +
  theme_minimal() +
  scale_color_manual(values=c('white', 'black')) +
  xlab('')+
  ylab('SMLSE')+
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
         axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
         axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
         axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain")
        )
ggsave('vis/DE_speakers.png', speaker_plot, height = 7, width = 13)


## aggregate per party
partycols=c("lightblue", "black", "#FF6565", "gold", "lightgreen", "firebrick", 'blue')
de_pt <- de %>% 
  group_by(party) %>% 
  summarise(mean_w = weighted.mean(x=afd_pred, w=n_words_raw),
            sd_w = sqrt(wtd.var(x=afd_pred, weights=n_words_raw, normwt = T)),
            n_speeches = length(afd_pred))
de_pt$se <- de_pt$sd_w/sqrt(de_pt$n_speeches)
de_pt$ci_low <- de_pt$mean_w - 1.96*de_pt$se
de_pt$ci_up <- de_pt$mean_w + 1.96*de_pt$se

# plot
party_plot <- ggplot(de_pt, aes(x=mean_w, y=party, xmin = ci_low, xmax = ci_up, col = party)) + 
  geom_point(show.legend = F, size = 2) + 
  geom_linerange(size = 1, show.legend = F) +
  scale_color_manual(values=partycols) +
  scale_y_discrete(limits=rev(c("AfD", "CDU/CSU", "SPD", "FDP", "GRUENE", "PDS/LINKE", "independent")))+
  xlab('SMLSE')
ggsave('vis/DE_parties.png', party_plot, width = 6, height=3)


# plot distribution per party
party_dens_plot <- ggplot(de, aes(x=afd_pred, y=party, fill = party)) + 
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=de_pt, aes(x = mean_w, y=party), show.legend = F)+
  geom_linerange(data=de_pt, aes(xmin = ci_low, xmax = ci_up, y=party), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  xlab('SMLSE')
ggsave('vis/DE_parties_density.png', party_dens_plot, width = 6, height=3)






## aggregate per month and party
de$my <- floor_date(de$date, "month")

de_pt_my <- de %>% 
  group_by(party, my) %>% 
  summarise(mean_w = weighted.mean(x=afd_pred, w=n_words_raw),
            sd_w = sqrt(wtd.var(x=afd_pred, weights=n_words_raw, normwt = T)),
            n_speeches = length(afd_pred))
de_pt_my$se <- de_pt_my$sd_w/sqrt(de_pt_my$n_speeches)
de_pt_my$ci_low <- de_pt_my$mean_w - 1.96*de_pt_my$se
de_pt_my$ci_up <- de_pt_my$mean_w + 1.96*de_pt_my$se

# plot

de_pt_my %>% 
  filter(party != 'independent') %>% 
  ggplot(aes(x = my, y = mean_w, col=party, fill=party, ymin = ci_low, ymax = ci_up)) + 
  geom_line() +
  geom_ribbon(alpha=0.2) +
  scale_color_manual(values=partycols) +
  scale_fill_manual(values=partycols)+
  ylab('SMLSE')




## show most and least polarised debates

# excluude independents
de_noind <- de %>% 
  filter(party != 'independent')
  

# aggregate party X debate
de_pt_ag <- de_noind %>% 
  group_by(party, agenda) %>%
  summarise(mean_w = weighted.mean(x=afd_pred, w=n_words_raw),
            sd_w = sqrt(wtd.var(x=afd_pred, weights=n_words_raw, normwt = T)),
            n_speeches = length(afd_pred))
de_pt_ag$se <- de_pt_ag$sd_w/sqrt(de_pt_ag$n_speeches)
de_pt_ag$ci_low <- de_pt_ag$mean_w - 1.96*de_pt_ag$se
de_pt_ag$ci_up <- de_pt_ag$mean_w + 1.96*de_pt_ag$se


# get agendas where all 6 parties gave at least 2 speeches
full_pt_agendas <- de_pt_ag %>% 
  filter(n_speeches > 3) %>%
  group_by(agenda) %>% 
  summarise(n_pt = length(mean_w)) %>% 
  filter(n_pt == 6) %>% 
  select(agenda)


# subset to agendas identified
de_pt_ag_r <- de_pt_ag %>% 
  filter(agenda %in% full_pt_agendas$agenda)

# aggregate and sort by afd/not afd distance, identify most and least polarised issues
de_pt_ag_r$afd <- de_pt_ag_r$party == 'AfD'

least <- de_pt_ag_r %>% 
  group_by(afd, agenda) %>% 
  summarise(mean = wtd.mean(mean_w, weights = n_speeches)) %>% 
  group_by(agenda) %>% 
  summarise(dist = max(mean)-min(mean)) %>% 
  arrange(dist) %>% 
  head()

most <- de_pt_ag_r %>% 
  group_by(afd, agenda) %>% 
  summarise(mean = wtd.mean(mean_w, weights = n_speeches)) %>% 
  group_by(agenda) %>% 
  summarise(dist = max(mean)-min(mean)) %>% 
  arrange(-dist) %>% 
  head()



# plot
# plot each debate
for  (ag in (1:length(least$agenda))){
    plot <-
      ggplot(de_noind[de$agenda==least$agenda[ag],], aes(x = afd_pred, y = party, fill = party, color = party))+
      geom_density_ridges(alpha=0.2, show.legend = F)+
      geom_point(data=de_pt_ag[de_pt_ag$agenda==least$agenda[ag],], aes(x = mean_w, y=party), show.legend = F, inherit.aes = F)+
      geom_linerange(data=de_pt_ag[de_pt_ag$agenda==least$agenda[ag],], aes(xmin = ci_low, xmax = ci_up, y=party), inherit.aes = F)+
      scale_fill_manual(values=partycols)+
      scale_color_manual(values=partycols)+
      ggtitle(label = ag, subtitle = least$agenda[ag])
    ggsave(paste0('vis/debates/de_least_', ag, '.png'), plot)
}


for  (ag in (1:length(most$agenda))){
  plot <-
    ggplot(de_noind[de$agenda==most$agenda[ag],], aes(x = afd_pred, y = party, fill = party, color = party))+
    geom_density_ridges(alpha=0.2, show.legend = F)+
    geom_point(data=de_pt_ag[de_pt_ag$agenda==most$agenda[ag],], aes(x = mean_w, y=party), show.legend = F, inherit.aes = F)+
    geom_linerange(data=de_pt_ag[de_pt_ag$agenda==most$agenda[ag],], aes(xmin = ci_low, xmax = ci_up, y=party), inherit.aes = F)+
    scale_fill_manual(values=partycols)+
    scale_color_manual(values=partycols)+
    ggtitle(label = ag, subtitle = most$agenda[ag])
  ggsave(paste0('vis/debates/de_most_', ag, '.png'), plot)
}


# selected issues
plot <-
  ggplot(de_noind[de$agenda==most$agenda[6],], aes(x = afd_pred, y = party, fill = party, color = party))+
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=de_pt_ag[de_pt_ag$agenda==most$agenda[6],], aes(x = mean_w, y=party), show.legend = F, inherit.aes = F)+
  geom_linerange(data=de_pt_ag[de_pt_ag$agenda==most$agenda[6],], aes(xmin = ci_low, xmax = ci_up, y=party), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  scale_color_manual(values=partycols)+
  ggtitle(label = 'First debate on the budget proposal', subtitle = '15/05/2008')+
  xlab('SMLSE')+
  ylab('')+
  scale_x_continuous(limits=c(0,1))
ggsave(paste0('vis/debates/de_most_selected.png'), plot)

plot <-
  ggplot(de_noind[de$agenda==least$agenda[1],], aes(x = afd_pred, y = party, fill = party, color = party))+
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=de_pt_ag[de_pt_ag$agenda==least$agenda[1],], aes(x = mean_w, y=party), show.legend = F, inherit.aes = F)+
  geom_linerange(data=de_pt_ag[de_pt_ag$agenda==least$agenda[1],], aes(xmin = ci_low, xmax = ci_up, y=party), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  scale_color_manual(values=partycols)+
  ggtitle(label = 'Debate on organ donations', subtitle = '28/11/2008')+
  xlab('SMLSE')+
  ylab('')+
  scale_x_continuous(limits=c(0,1))
ggsave(paste0('vis/debates/de_least_selected.png'), plot)


