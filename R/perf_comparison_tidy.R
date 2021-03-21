source(here::here("R", "utilities.R"))

# Load data, create subdirectories =============================================
perf <- list(
  CCNS = loadRData(here("data/cces-tidy/perf_summ_CCES_Nationscape.Rda")) %>%
    mutate(Survey = ifelse(Year == 2020, "NS", "CCES")),
  ANES = loadRData(here("data/anes-tidy/perf_summ_ANES_prezvote_rf.Rda")) %>%
    mutate(Survey = "ANES")
) %>%
  bind_rows()

if (!dir.exists(here("tab/avg"))) {
  c("avg", "selected", "reg", "diff") %>%
    map(~ dir.create(here("tab", .x)))
}

# If put into simple linear regression =========================================
for (metric in c("Accuracy", "AUC", "F1")) {
  cross2(
    c("ANES", "CCES", "NS", "all"),
    setNames(set_labels, nm = paste0("set", seq(8)))
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

        # First, average of metric =============================================
        write(
          round(mean(temp[[metric]]) * 100, digits = 1),
          file = here(
            "tab", "avg",
            paste0(
              .x[[1]], "_avg_", ifelse(grepl("PID", .x[[2]]), "set2", "set1"),
              "_", tolower(metric), ".tex"
            )
          )
        )

        # Then a linear slope ==================================================
        if (.x[[1]] %in% c("ANES", "all")) {
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
                      "tab", "reg",
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
      }
    )
}

# Some special cases to mention ================================================
for (yr in c(1960, 1972, 2000, 2008)) {
  write(
    perf %>%
      filter(Set == "Demographics Only" & Year == yr & Survey == "ANES") %>%
      .$Accuracy %>% {
        round(. * 100, digits = 1)
      },
    file = here("tab", "selected", paste0("ANES_", yr, "_accuracy_set1.tex"))
  )
}

for (yr in c(2008, 2016, 2020)) {
  write(
    perf %>%
      filter(Set == "Demographics Only" & Year == yr & Survey != "ANES") %>%
      .$Accuracy %>% {
        round(. * 100, digits = 1)
      },
    file = here("tab", "selected", paste0("CCES_", yr, "_accuracy_set1.tex"))
  )
}

# Increase in accuracy from Spec. 2 ============================================
for (metric in c("Accuracy", "AUC", "F1")) {
  cross2(
    c("ANES", "CCES", "NS", "all"),
    c(set3 = "Demo. + PID + Issues", set4 = "All Covariates")
  ) %>%
    map(
      ~ {
        if (.x[[1]] == "all") {
          temp <- perf
        } else {
          temp <- perf %>% filter(Survey == .x[[1]])
        }

        round(
          (mean(temp %>% filter(Set == .x[[2]]) %>% .[[metric]]) -
            mean(temp %>% filter(Set == "Demo. + PID") %>% .[[metric]])) * 100,
          digits = 1
        ) %>%
          write(
            .,
            file = here(
              "tab", "diff",
              paste0(
                .x[[1]], "_avg_", ifelse(grepl("PID", .x[[2]]), "set3", "set4"),
                "_relative_to_set2_", tolower(metric), ".tex"
              )
            )
          )
      }
    )
}

# Increase in accuracy from Spec. 2 decreasing over time? ======================
for (metric in c("Accuracy", "AUC", "F1")) {
  cross2(
    c("ANES", "CCES", "all"),
    c(set3 = "Demo. + PID + Issues", set4 = "All Covariates")
  ) %>%
    map(
      ~ {
        if (.x[[1]] == "all") {
          temp <- perf
        } else {
          temp <- perf %>% filter(Survey == .x[[1]])
        }

        lm_out <- temp %>%
          filter(Set == .x[[2]] | Set == "Demo. + PID") %>%
          pivot_wider(
            id_cols = c(Year, Survey),
            names_from = Set, values_from = all_of(metric)
          ) %>%
          mutate(
            gap = !!as.name(.x[[2]]) - `Demo. + PID`
          ) %>%
          lm(gap ~ Year, data = .) %>%
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
                    "tab", "reg",
                    paste0(
                      .x[[1]], "_ts_",
                      ifelse(grepl("PID", .x[[2]]), "set3", "set4"),
                      "_", tolower(metric),
                      "_slope_convergence_", y, ".tex"
                    )
                  )
                )
            }
          )
      }
    )
}
