source(here::here("R", "utilities.R"))
load(here("output/ANES/ANES_perf_pid.RData"))
load(here("output/ANES/ANES_varimp_pid.RData"))

perf_summ <- function(perf, dv, method, set, yr = anes_years) {
  data.frame(
    Year = yr,
    Accuracy = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map_dbl(~ .x$cf.matrix$overall[["Accuracy"]]),
    Accuracy_lower = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map_dbl(~ .x$cf.matrix$overall[["AccuracyLower"]]),
    Accuracy_upper = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map_dbl(~ .x$cf.matrix$overall[["AccuracyUpper"]]),
    CI = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$cf.matrix$overall) %>%
      map(
        ~ paste0(
          "[",
          str_pad(round(.x[["AccuracyLower"]], digits = 4), 6, "right", "0"),
          ", ",
          str_pad(round(.x[["AccuracyUpper"]], digits = 4), 6, "right", "0"),
          "]"
        )
      ) %>%
      unlist(),
    AUC = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map_dbl(~ .x$multiclass[["AUC"]]),
    prAUC = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map_dbl(~ .x$multiclass[["prAUC"]]),
    Mean_Sensitivity = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map_dbl(~ .x$multiclass[["Mean_Sensitivity"]]),
    Mean_Specificity = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map_dbl(~ .x$multiclass[["Mean_Specificity"]]),
    Mean_Recall = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map_dbl(~ .x$multiclass[["Mean_Recall"]]),
    Mean_Balanced_Accuracy = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map_dbl(~ .x$multiclass[["Mean_Balanced_Accuracy"]]),
    row.names = NULL
  )
}

cross2(c("prezvote"), c("logit", "rf")) %>%
  map(
    ~ {
      summ_df <- seq(2) %>%
        map(
          function(x) perf_summ(perf, .x[[1]], .x[[2]], x, yr = anes_years)
        ) %>%
        bind_rows(.id = "Y") %>%
        mutate(
          Y = factor(Y, levels = seq(2), labels = c("7-pt PID", "3-pt PID"))
        )

      save(
        summ_df,
        file = here(
          "output", "ANES",
          paste0("perf_summ_ANES_", .x[[1]], "_", .x[[2]], "_pid.Rda")
        )
      )
    }
  )

width <- 6.2
height <- 4
# plot_temp <- function(y = "pid7", ...) {
#   p <- pdf_default(
#     po_full(
#       perf %>%
#         mutate(Set = "Demographics Only") %>%
#         filter(Y == y),
#       metric = metric, vdir = 1, colour_nrow = 1, linetype_nrow = 1, ...
#     )
#   ) +
#     theme(legend.position = "bottom", legend.key.width = unit(.9, "cm")) +
#     scale_y_continuous(limits = c(0, 1))
#   if (metric != "Accuracy" & metric != "AUC") {
#     p <- p + scale_y_continuous(limits = c(0, 1))
#   }
#   return(p)
# }

plot_temp <- function(y = "pid7", ...) {
  p <- pdf_default(
    po_full(
      perf %>% mutate(Set = "Demographics Only"),
      metric = metric, vdir = 1, colour_nrow = 1, linetype_nrow = 2, ...
    )
  ) +
    theme(legend.position = "bottom", legend.key.width = unit(.9, "cm")) +
    scale_y_continuous(limits = c(0, 1))
  if (metric != "Accuracy" & metric != "AUC") {
    p <- p + scale_y_continuous(limits = c(0, 1))
  }
  return(p)
}

perf <- loadRData(here("output/ANES/perf_summ_ANES_prezvote_rf_pid.Rda")) %>%
  rename(Survey = Y)
for (metric in c("Accuracy", "AUC", "prAUC")) {
  # for (pid in c("pid3", "pid7")) {
  #   pdf(
  #     here(
  #       "fig",
  #       paste0("survey_rf_", tolower(metric), "_ts_", pid, "_predict.pdf")
  #     ),
  #     width = width, height = (height + 0.5)
  #   )
  #   print(
  #     # https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
  #     plot_temp() +
  #       scale_colour_manual(
  #         values = viridisLite::viridis(2, end = .85),
  #         name = "Specification"
  #       )
  #   )
  #   dev.off()
  # }
  pdf(
    here("fig", paste0("survey_rf_", tolower(metric), "_ts_pid_predict.pdf")),
    width = width, height = (height + 0.5)
  )
  print(
    # https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
    plot_temp() +
      scale_colour_manual(
        values = viridisLite::viridis(2, end = .85),
        name = "Specification"
      ) +
      guides(linetype = guide_legend(title = "Variable"))
  )
  dev.off()
}

c("7-pt PID", "3-pt PID") %>%
  set_names(., nm = c("pid7", "pid3")) %>%
  map(
    ~ perf %>%
      filter(Survey == .x) %>%
      select(Year, Accuracy_lower, Accuracy_upper, Accuracy, Survey) %>%
      ggplot(aes(x = Year, y = Accuracy, color = Survey, shape = Survey)) +
      geom_pointrange(aes(ymin = Accuracy_lower, ymax = Accuracy_upper)) +
      scale_x_continuous(breaks = c(anes_years, 2020)) +
      scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, 0.1)) +
      scale_color_viridis_d(end = 0.85) +
      guides(linetype = guide_legend(title = "Variable"))
  ) %>%
  imap(
    ~ {
      pdf(
        here(paste0("fig/pid_rf_accrange_ts_", .y, ".pdf")),
        width = 6.8, height = height
      )
      print(pdf_default(.x))
      dev.off()
    }
  )

# Summary and Regression =======================================================
perf %>% filter(Survey == "7-pt PID") %>% .$Accuracy %>% mean() ## 0.2667
perf %>% filter(Survey == "3-pt PID") %>% .$Accuracy %>% mean() ## 0.5773

perf %>% 
  filter(Survey == "7-pt PID") %>%
  lm(Accuracy ~ Year, data = .) %>%
  broom::tidy()

# A tibble: 2 x 5
# term         estimate    std.error statistic p.value
# <chr>           <dbl>        <dbl>     <dbl>   <dbl>
# 1 (Intercept) -1.41      0.682        -2.07   0.0567
# 2 Year         0.000845  0.000344      2.46   0.0267

perf %>% 
  filter(Survey == "3-pt PID") %>%
  lm(Accuracy ~ Year, data = .) %>%
  broom::tidy()

# A tibble: 2 x 5
# term           estimate std.error statistic p.value
# <chr>             <dbl>     <dbl>     <dbl>   <dbl>
# 1 (Intercept) -0.955     0.973       -0.981   0.342
# 2 Year         0.000772  0.000491     1.57    0.136

## Baseline
anes <- read_dta(here("data/anes/anes_timeseries_cdf.dta")) %>% 
  filter(VCF0004 != 1948 & !is.na(VCF0303))

baseline_pid7 <- as.numeric(prop(anes, "VCF0301")) / 100
sum(baseline_pid7 * baseline_pid7) ## 0.1533: 0.2667 - 0.1533 = 0.1134 increase

baseline_pid3 <- as.numeric(prop(anes, "VCF0303")) / 100
sum(baseline_pid3 * baseline_pid3) ## 0.4150: 0.5773 - 0.4150 = 0.1623 increase

