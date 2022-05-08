# Purpose: Calculate 2-party vote shares (1952-2020)

library(tidyverse)
library(knitr)
library(kableExtra)

EL <- read_csv(here::here("tab", "elections", "presidential_elections.csv"))

EL$dem2P <-
  EL$dem_vote_share_Leip / (EL$dem_vote_share_Leip + EL$rep_vote_share_Leip)
EL$rep2P <-
  EL$rep_vote_share_Leip / (EL$dem_vote_share_Leip + EL$rep_vote_share_Leip)

EL$Winner_2P <- ifelse(EL$dem2P > EL$rep2P, EL$dem2P, EL$rep2P)

mean(EL$Winner_2P)
# The average two-party winner vote share is: 0.5416623

mean(EL$Winner_2P[EL$year >= 2000])

EL %>%
  mutate(Winner_2P = Winner_2P * 100) %>%
  select(year, Winner_2P) %>%
  kable(
    col.names = c(
      "Election",
      "Accuracy achieved by a naive guess (i.e. popular vote winner)"
    ),
    digits = 2,
    booktabs = T,
    format = "latex"
  ) %>%
  save_kable(here::here("tab", "elections", "benchmarks.text"),
    keep_tex = T
  )