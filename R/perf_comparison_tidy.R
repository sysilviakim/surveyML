source(here::here("R", "utilities.R"))

perf <- list(
  CCNS = loadRData(here("data/cces-tidy/perf_summ_CCES_Nationscape.Rda")) %>%
    mutate(Survey = "CCES/NS"),
  ANES = loadRData(here("data/anes-tidy/perf_summ_ANES.Rda")) %>%
    mutate(Survey = "ANES")
) %>%
  bind_rows()

# If put into simple linear regression =========================================
for (metric in c("Accuracy", "AUC", "F1")) {
  cross2(
    c("ANES", "all"), c(set1 = "Demographics Only", set2 = "Demo. + PID")
  ) %>%
    map(
      ~ {
        if (.x[[1]] == "all") {
          temp <- perf
        } else {
          temp <- perf %>% filter(Survey == .x[[1]])
        }
        temp <- temp %>%
          filter(Set == .x[[2]])

        lm_out <- temp %>%
          lm(sprintf("%s ~ Year", metric), data = .) %>%
          broom::tidy()

        c(est = "estimate", se = "std.error", pvalue = "p.value") %>%
          imap(
            function(x, y) {
              lm_out %>%
                filter(term == "Year") %>%
                .[[x]] %>%
                round(
                  .,
                  digits = ifelse(
                    x == "p.value",
                    max(
                      2,
                      str_match_all(
                        formatC(., format = "e"),
                        "-([0-9]+)"
                      )[[1]][1, 2] %>% as.numeric()
                    ),
                    max(
                      4,
                      str_match_all(
                        formatC(., format = "e"),
                        "-([0-9]+)"
                      )[[1]][1, 2] %>% as.numeric()
                    )
                  )
                ) %>%
                write(
                  .,
                  file = here(
                    "tab",
                    paste0(
                      .x[[1]], "_ts_",
                      ifelse(grepl("PID", .x[[2]]), "set2", "set1"),
                      "_", tolower(metric),
                      "_slope_", y, ".tex"
                    )
                  )
                )
            }
          )
      }
    )
}
