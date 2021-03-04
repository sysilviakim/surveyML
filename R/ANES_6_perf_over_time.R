source(here::here("R", "utilities.R"))
load(here("output/ANES/ANES_perf.RData"))
load(here("output/ANES/ANES_varimp.RData"))
anes_years <- seq(1952, 2016, by = 4)


cross2(c("prezvote", "house", "senate"), c("logit", "cart", "rf")) %>%
  map(
    ~ {
      summ_df <- seq(4) %>%
        map(
          function(x) {
            perf_summ(
              perf, .x[[1]], .x[[2]], x,
              yr = seq(1952, 2016, 4)
            )
          }
        ) %>%
        bind_rows(.id = "Set") %>%
        mutate(Set = factor(Set, levels = seq(4), labels = set_labels))

      save(
        summ_df,
        file = here(
          "data", "anes-tidy",
          paste0("perf_summ_ANES_", .x[[1]], "_", .x[[2]], ".Rda")
        )
      )
    }
  )
