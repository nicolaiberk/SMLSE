# make plot of weights

library(tidyverse)
library(ggplot2)

setwd("C:/Users/nicol/Dropbox/Studium/Amsterdam/Studies/Semester 4/Master Thesis/GitHub/SMLSE/DE")

## load weights
pre_dta  <- read_csv("smlse/DE_weights.csv") %>%
  mutate(weight_sq = weight**2) %>%
  ## filter top
  slice_max(weight_sq, n = 30) %>% ## plot
  ## sort by weight
  arrange(desc(weight))

pre_dta$lbls_en <- 
  c(
    "AfD",
    "german",
    "Germany",
    "old-parties",
    "here",
    "government",
    "Merkel",
    "a lot",
    "thank",
    "one",
    "thanks",
    "voters",
    "EU",
    "german ",
    "colleagues",
    "ask",
    "important",
    "coalition agreement",
    "warmly",
    "debates",
    "the left",
    "ciizens [f]",
    "hence",
    "last",
    "therefor",
    "say",
    "democrats",
    "find",
    "need",
    "colleagues [f]"

    )

pre_dta %>%
  ggplot(aes(x = reorder(lbls_en, weight), y = weight, fill = weight)) +
  geom_col() +
  labs(x = "Feature", y = "Weight") +
  theme_minimal() +
  coord_flip() +
  ggtitle("DE") +
  scale_fill_gradient2(
    midpoint = 0,
    low = "red",
    mid = "white",
    high = "green",
    space = "Lab") +
    theme(legend.position = "None")

## save plot
ggsave(filename = "vis/DE_weights.png", width = 3, height = 6)
