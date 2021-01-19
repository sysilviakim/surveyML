source("R/utilities.R")
load("output/ANES/ANES_perf.RData")
load("output/ANES/ANES_varimp.RData")
anes_years <- seq(1952, 2016, by = 4)

summ_df <- seq(4) %>%
  map(~ perf_summ(perf, 1, "rf", .x, yr = seq(1952, 2016, 4))) %>%
  bind_rows(.id = "Set") %>%
  mutate(Set = factor(Set, levels = seq(4), labels = set_labels))

save(summ_df, file = "data/anes-tidy/perf_summ_ANES.Rda")