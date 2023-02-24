# make plot of weights

library(tidyverse)
library(ggplot2)

setwd("C:/Users/nicol/Dropbox/Studium/Amsterdam/Studies/Semester 4/Master Thesis/GitHub/SMLSE/AT")

## load weights
pre_dta  <- read_csv("smlse/AT_weights.csv") %>%
  mutate(weight_sq = weight**2) %>%
  ## filter top
  slice_max(weight_sq, n = 30) %>% ## plot
  ## sort by weight
  arrange(desc(weight))

pre_dta$lbls_en <- 
  c(
    "honored",
    "SPÖ",
    "once",
    "patients [m]",
    "years",
    "just",
    "population",
    "Vienna",
    "high",
    "freedom",
    "colleague [f]",
    "yes",
    "Pilz (Greens)",
    "area",
    "also",
    "Kern (SPÖ)",
    "want",
    "appropriate",
    "finance minister",
    "Kurz (ÖVP)",
    "colleagues [m]",
    "citizens [f]",
    "motion",
    "love",
    "minister [f]",
    "humans",
    "FPÖ",
    "colleagues [f]",
    "federal minister [f]",
    "ÖVP"
  )

pre_dta %>%
  ggplot(aes(x = reorder(lbls_en, weight), y = weight, fill = weight)) +
  geom_col() +
  labs(x = "Feature", y = "Weight") +
  theme_minimal() +
  coord_flip() +
  ggtitle("AT") +
  scale_fill_gradient2(
    midpoint = 0,
    low = "red",
    mid = "white",
    high = "green",
    space = "Lab") +
    theme(legend.position = "None")

## save plot
ggsave(filename = "vis/AT_weights.png", width = 3, height = 6)
