# make plot of weights

library(tidyverse)
library(ggplot2)

setwd("C:/Users/nicol/Dropbox/Studium/Amsterdam/Studies/Semester 4/Master Thesis/GitHub/SMLSE/NL")

## load weights
pre_dta  <- read_csv("smlse/NL_weights.csv") %>%
  mutate(weight_sq = weight**2) %>%
  ## filter top
  slice_max(weight_sq, n = 30) %>% ## plot
  ## sort by weight
  arrange(desc(weight))

pre_dta$lbls_en <- 
  c(
    "and so on",
    "immigration",
    "islam",
    "PVV",
    "islamic",
    "Brussels",
    "cuts",
    "possible",
    "illegal immigrants",
    "immigration policy",
    "hate-speech law",
    "gigantic",
    "animal police",
    "status holders",
    "signing",
    "budget",
    "house-keeping",
    "EU",
    "consideration",
    "agriculture",
    "at the same time",
    "noting",
    "careful",
    "agreements",
    "Agema (PVV)",
    "mister",
    "look",
    "bright",
    "predominant",
    "Baudet (FvD)"
    )

pre_dta %>%
  ggplot(aes(x = reorder(lbls_en, weight), y = weight, fill = weight)) +
  geom_col() +
  labs(x = "Feature", y = "Weight") +
  theme_minimal() +
  coord_flip() +
  ggtitle("NL") +
  scale_fill_gradient2(
    midpoint = 0,
    low = "red",
    mid = "white",
    high = "green",
    space = "Lab") +
    theme(legend.position = "None")

## save plot
ggsave(filename = "vis/NL_weights.png", width = 3, height = 6)
