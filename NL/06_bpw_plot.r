# make plot of weights

library(tidyverse)
library(ggplot2)

setwd("C:/Users/nicol/Dropbox/Studium/Amsterdam/Studies/Semester 4/Master Thesis/GitHub/SMLSE/NL")

## load weights
plt  <- read_csv("smlse/NL_weights.csv") %>%
  mutate(weight_sq = weight**2) %>%
  ## filter top
  slice_max(weight_sq, n = 30) %>% ## plot
  ## sort by weight
  arrange(desc(weight)) %>%
  ggplot(aes(x = reorder(feature, weight), y = weight, fill = weight)) +
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
    space = "Lab")

## save plot
ggsave(filename = "vis/NL_weights.png", width = 4, height = 10)
