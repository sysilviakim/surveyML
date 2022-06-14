source(here::here("R", "utilities.R"))
load(here("output", "summ_list.Rda"))
load(here("output", "summ_list_pid.Rda"))
load(here("output/ANES/ANES_perf.Rda"))

if (!dir.exists(here("tab/avg"))) {
  dir.create(here("tab/avg"), recursive = TRUE)
  dir.create(here("tab/reg"), recursive = TRUE)
}

# Settings =====================================================================
plot_temp <- function(df, sets = seq(4), ...) {
  p <- pdf_default(
    po_full(
      df[["rf"]] %>%
        rename(Set = `Variable Specification`) %>%
        filter(Set %in% set_labels[sets]),
      metric = metric, vdir = 1, ...
    )
  ) +
    theme(legend.position = "bottom", legend.key.width = unit(.9, "cm")) +
    scale_y_continuous(limits = c(0.4, 1))
  if (metric != "Accuracy" & metric != "AUC") {
    p <- p + scale_y_continuous(limits = c(0, 1))
  }
  return(p)
}

# Performance summary in list ==================================================
perf_list <- methods %>%
  map(
    ~ bind_rows(
      summ_df[[.x]] %>% mutate(Set = "Vote Choice"),
      summ_df_pid[[.x]] %>%
        filter(`Variable Specification` == "Binary PID, Demographics Only") %>%
        mutate(Set = "Binary PID") %>%
        mutate(`Variable Specification` = "Demographics Only")
    ) %>%
      filter(`Variable Specification` == "Demographics Only")
  )

# Average calculation/slope regression =========================================
for (method in c("logit", "cart", "rf")) {
  for (metric in c("Accuracy", "AUC", "F1")) {
    for (yy in c("vote choice", "pid")) {
      if (yy == "vote choice") {
        input <- set_labels %>%
          set_names(., nm = paste0("set", seq(length(set_labels))))
      } else {
        input <- set_labels_pid %>%
          set_names(
            .,
            nm = gsub(
              "-pt PID, Demo. Only, Broader Definition", "pid_set2",
              gsub(
                "-pt PID, Demographics Only", "pid_set1",
                gsub("Binary PID", "2-pt PID", set_labels_pid)
              )
            )
          )
      }

      input %>%
        imap(
          ~ {
            if (yy == "vote choice") {
              x <- summ_df[[method]] %>%
                filter(`Variable Specification` == .x)
            } else {
              x <- summ_df_pid[[method]] %>%
                filter(`Variable Specification` == .x)
            }

            ### average
            write(
              formatC(
                mean(x[[metric]], na.rm = TRUE) * 100,
                digits = 1, format = "f"
              ),
              file = here(
                "tab", "avg",
                ifelse(
                  yy == "vote choice",
                  paste0(
                    "ANES", "_avg_vote_choice_", .y, "_", tolower(metric), "_",
                    tolower(method), ".tex"
                  ),
                  paste0(
                    "ANES", "_avg_", .y, "_", tolower(metric), "_",
                    tolower(method), ".tex"
                  )
                )
              )
            )

            if (metric != "F1") {
              ### linear regression
              lm_out <- lm(sprintf("%s ~ Year", metric), data = x) %>%
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
                            4,
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
                          ifelse(
                            yy == "vote choice",
                            paste0(
                              "ANES", "_ts_slope_vote_choice_",
                              .y, "_", tolower(metric),
                              "_", tolower(method), "_", y, ".tex"
                            ),
                            paste0(
                              "ANES", "_ts_slope_", .y, "_", tolower(metric),
                              "_", tolower(method), "_", y, ".tex"
                            )
                          )
                        )
                      )
                  }
                )
            }
          }
        )
    }
  }
}

## Special cases
c(1960, 1972, 2000, 2008) %>%
  map(
    ~ {
      x <- summ_df$rf %>%
        filter(`Variable Specification` == "Demographics Only" & Year == .x) %>%
        .$Accuracy
      write(
        formatC(x * 100, digits = 1, format = "f"),
        file = here(
          "tab", "avg", paste0("ANES_", .x, "_vote_choice_accuracy_set1.tex")
        )
      )
    }
  )

## Relative improvement
temp <- summ_df$rf %>%
  rename(spec = `Variable Specification`) %>%
  group_by(spec) %>%
  summarise(Accuracy = mean(Accuracy))

seq(3, 4) %>%
  map(
    ~ write(
      formatC(
        mean(temp$Accuracy[.x] - temp$Accuracy[2], na.rm = TRUE) * 100,
        digits = 1, format = "f"
      ),
      file = here(
        "tab", "avg",
        paste0("ANES_avg_vote_choice_set", .x, "_relative_to_set2_accuracy.tex")
      )
    )
  )

# Fig 1A and 1B: Demo Accuracy, 2 y-variables ==================================
methods %>%
  map(
    ~ {
      p <- po_full(
        perf_list[[.x]], "Accuracy",
        ylim = c(0, 1), name = "Outcome Variable", y2 = TRUE,
        colour_nrow = 1, linetype_nrow = 1, end = 0.85, accrange = TRUE
      ) +
        ylab("ANES Wave")
      p <- pdf_default(p) +
        theme(legend.position = "bottom", legend.key.width = unit(.9, "cm"))
      p

      pdf(
        here("fig", paste0("survey_", .x, "_accrange.pdf")),
        width = 6.2, height = 4
      )
      print(p)
      dev.off()
    }
  )

# Fig 3: Performance measures over time ========================================
for (metric in c("Accuracy", "AUC", "Precision", "Recall", "F1")) {
  pdf(
    here("fig", paste0("survey_votechoice_rf_", tolower(metric), "_ts.pdf")),
    width = width, height = height
  )
  print(plot_temp(summ_df))
  dev.off()
}

# SI figures (Vote Choice ROC) =================================================
anes_sets %>%
  map(
    ~ {
      pdf(
        here(
          "fig", "ANES", paste0("ANES_perf_set", .x, ".pdf")
        ),
        width = 6, height = 5
      )
      grid_arrange_shared_legend(
        list = roc_comparison(perf, set = .x) %>%
          imap(
            ~ pdf_default(.x + ggtitle(gsub("year", "", .y))) +
              theme(
                axis.title = element_blank(),
                plot.title = element_text(size = 10)
              ) +
              scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
              scale_y_continuous(breaks = c(0, 0.5, 1.0))
          ),
        ncol = 5, nrow = 4
      )
      dev.off()
    }
  )

# SI figures (alternative demo. only) ==========================================
for (metric in c("Accuracy", "AUC", "Precision", "Recall", "F1")) {
  pdf(
    here("fig", paste0("survey_rf_", tolower(metric), "_ts_pid_ideology.pdf")),
    width = width, height = (height + 0.5)
  )
  print(
    # https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
    plot_temp(summ_df, sets = c(2, 3, 8), colour_nrow = 3, end = 1) +
      scale_colour_manual(
        values = c(
          # Force as the third scale in the default figure
          viridisLite::viridis(4, end = .9)[2],
          viridisLite::viridis(9, end = .9)[7],
          viridisLite::viridis(9, end = 1)[9]
        ),
        name = "Specification"
      )
  )
  dev.off()

  pdf(
    here(
      "fig",
      paste0("survey_rf_", tolower(metric), "_ts_religion_south.pdf")
    ),
    width = width, height = height
  )
  print(
    plot_temp(summ_df, sets = c(1, 5, 6), colour_nrow = 3, end = 1) +
      scale_colour_manual(
        values = c(
          viridisLite::viridis(9, end = 1)[1],
          viridisLite::viridis(9, end = 1)[8],
          viridisLite::viridis(9, end = 1)[5]
        ),
        name = "Specification"
      )
  )
  dev.off()
}
