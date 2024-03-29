###########################################################
# MEASURING RHETORICAL SIMILARITY WITH SUPERVISED LEARNING
# VISUALISATION OF GERMAN ESTIMATES
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
library(stringr)


options(stringsAsFactors = FALSE)

# import fonts
font_import(pattern = '.*verdana.*', prompt = F)
loadfonts(device = "win", quiet = T)
windowsFonts(family = 'Verdana')

# Import
setwd("DE")
de <- read.csv('smlse/DE_notext.csv', fileEncoding = 'UTF-8')
de$date <- as.Date(de$date)

# define groups and partycolors
de <- de[!de$party %in% c(''),]

de$party <- factor(de$party, levels = c("AfD", "CDU/CSU", "SPD", "FDP", "GRUENE", "PDS/LINKE", "independent"))

partycols=c("lightblue", "grey", "#FF6565", "gold", "lightgreen", "firebrick", 'blue')



## aggregate per speaker
de_sp <- de %>% 
  group_by(speaker, party) %>% 
  summarise(mean_w = weighted.mean(x=afd_pred, w=n_words_raw),
            sd_w = sqrt(wtd.var(x=afd_pred, weights=n_words_raw, normwt = T)),
            n_speeches = length(afd_pred),
            mean_w_r = weighted.mean(x=afd_ind_pred, w=n_words_raw),
            sd_w_r = sqrt(wtd.var(x=afd_ind_pred, weights=n_words_raw, normwt = T)),
            n_speeches_r = length(afd_ind_pred),
            n_words = sum(n_words_raw))
de_sp$se <- de_sp$sd_w/sqrt(de_sp$n_speeches)
de_sp$ci_low <- de_sp$mean_w - qt(1 - (0.05 / 2), de_sp$n_speeches-1)*de_sp$se
de_sp$ci_up <- de_sp$mean_w + qt(1 - (0.05 / 2), de_sp$n_speeches-1)*de_sp$se

de_sp$se_r <- de_sp$sd_w/sqrt(de_sp$n_speeches)
de_sp$ci_low_r <- de_sp$mean_w - qt(1 - (0.05 / 2), de_sp$n_speeches-1)*de_sp$se
de_sp$ci_up_r <- de_sp$mean_w + qt(1 - (0.05 / 2), de_sp$n_speeches-1)*de_sp$se


# plot
afd_members <- c('Alice Weidel', 'Frauke Petry') 
cdu_members <- c('Angela Merkel', 'Christian Freiherr von Stetten') 
spd_members <- c('Martin Schulz','Andrea Nahles')
fdp_members <- c('Christian Lindner', 'Karlheinz Busen')
green_members <- c('Annalena Baerbock', 'Renate Künast')
left_members <- c('Fabio De Masi', 'Katja Kipping')
future_ind <- c('Lars Herrmann', 'Uwe Kamann', 'Verena Hartmann', 'Frank Pasemann')
important_members <- c(afd_members, cdu_members, spd_members, fdp_members, green_members, left_members, future_ind)

de_sp$labels = ""
de_sp$labels[de_sp$speaker %in% important_members] <- de_sp$speaker[de_sp$speaker %in% important_members]
de_sp$labelcol = F
de_sp$labelcol[de_sp$speaker %in% important_members]<- T

de_sp$labels[de_sp$speaker %in% c('Lars Herrmann', 'Verena Hartmann')] <- ''

# write to csv for website
write.csv(file = 'smlse/DE_speakers.csv', x =  de_sp)


partycols=c("lightblue", "lightgrey", "#FF6565", "gold", "lightgreen", "firebrick", 'blue')

speaker_plot <-
  ggplot(de_sp[de_sp$n_words>100,],aes(x=party, y=mean_w, fill=party, label = labels, col = labelcol)) + 
  geom_dotplot(binwidth=0.015, method='histodot', drop = T, stackratio = .7, binaxis = 'y', stackdir = 'center', show.legend = F, stroke = 2)+
  geom_text(aes(label=labels), col='black', position = 'identity', size = 5, vjust=-1, family = 'Verdana') +
  scale_fill_manual(values=partycols) +
  scale_color_manual(values=c('white', 'black')) +
  theme_minimal() +
  xlab('')+
  ylab('Similarity to AfD') +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
         axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
         axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
         axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        text = element_text(family='Verdana'),
        plot.background = element_rect(fill = "#bfbfbf"))

ggsave('vis/DE_speakers.png', speaker_plot, height = 7, width = 13)
ggsave('vis/DE_speakers_presi.png', speaker_plot, height = 6, width = 10)



# larger and only last name labels, darker colors for PA
de_sp$labels <- word(de_sp$labels, -1)

speaker_plot_PA <-
  ggplot(de_sp[de_sp$n_words>100,],aes(x=party, y=mean_w, fill=party, label = labels, col = labelcol)) + 
  geom_dotplot(binwidth=0.015, method='histodot', drop = T, stackratio = .7, binaxis = 'y', stackdir = 'center', show.legend = F, stroke = 2.5)+
  geom_text(aes(label=labels), col='black', position = 'identity', size = 5, vjust=-1, family = 'Verdana') +
  scale_fill_manual(values=partycols) +
  theme_minimal() +
  scale_color_manual(values=c('white', 'black')) +
  xlab('')+
  ylab('Similarity to AfD')+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        text = element_text(family='Verdana'))

ggsave('vis/DE_speakers_PA.png', speaker_plot_PA, height = 5, width = 8)


# robustness check: predict AfD or independent authorship
speaker_plot_r_PA <-
  ggplot(de_sp[de_sp$n_words>100,],aes(x=party, y=mean_w_r, fill=party, label = labels, col = labelcol)) + 
  geom_dotplot(binwidth=0.015, method='histodot', drop = T, stackratio = .7, binaxis = 'y', stackdir = 'center', show.legend = F, stroke = 2.5)+
  geom_text(aes(label=labels), col='black', position = 'identity', size = 5, vjust=-1, family = 'Verdana') +
  scale_fill_manual(values=partycols) +
  theme_minimal() +
  scale_color_manual(values=c('white', 'black')) +
  xlab('')+
  ylab('Similarity to AfD')+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        text = element_text(family='Verdana'))

ggsave('vis/DE_speakers_r_PA.png', speaker_plot_r_PA, height = 5, width = 8)


## aggregate per party
partycols=c("lightblue", "black", "#FF6565", "gold", "lightgreen", "firebrick", 'blue')
de_pt <- de %>% 
  group_by(party) %>% 
  summarise(mean_w = weighted.mean(x=afd_pred, w=n_words_raw),
            sd_w = sqrt(wtd.var(x=afd_pred, weights=n_words_raw, normwt = T)),
            n_speeches = length(afd_pred))
de_pt$se <- de_pt$sd_w/sqrt(de_pt$n_speeches)
de_pt$ci_low <- de_pt$mean_w - qt(1 - (0.05 / 2), de_pt$n_speeches-1)*de_pt$se
de_pt$ci_up <- de_pt$mean_w + qt(1 - (0.05 / 2), de_pt$n_speeches-1)*de_pt$se

# plot
party_plot <- ggplot(de_pt, aes(x=mean_w, y=party, xmin = ci_low, xmax = ci_up, col = party)) + 
  geom_point(show.legend = F, size = 2) + 
  geom_linerange(size = 1, show.legend = F) +
  scale_color_manual(values=partycols) +
  scale_y_discrete(limits=rev(c("AfD", "CDU/CSU", "SPD", "FDP", "GRUENE", "PDS/LINKE", "")))+
  xlab('Similarity to AfD') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/DE_parties_mean.png', party_plot, width = 6, height=3)


# plot distribution per party
party_dens_plot <- ggplot(de, aes(x=afd_pred, y=party, fill = party)) + 
  geom_density_ridges(alpha=0.2, show.legend = F)+
  geom_point(data=de_pt, aes(x = mean_w, y=party), show.legend = F)+
  geom_linerange(data=de_pt, aes(xmin = ci_low, xmax = ci_up, y=party), inherit.aes = F)+
  scale_fill_manual(values=partycols)+
  xlab('Similarity to AfD') +
  theme(text = element_text(family='Verdana'))

ggsave('vis/DE_parties_density.png', party_dens_plot, width = 6, height=3)
