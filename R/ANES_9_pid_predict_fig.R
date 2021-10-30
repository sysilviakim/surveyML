source(here::here("R", "utilities.R"))
load(here("output/ANES/ANES_perf_pid.RData"))
load(here("output/ANES/ANES_varimp_pid.RData"))
anes_years <- as.numeric(gsub("year", "", names(perf)))

# Define functions =============================================================
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
    row.names = NULL
  )
}

width <- 6.2
height <- 4

plot_temp <- function(y = "pid7", ...) {
  p <- pdf_default(
    po_full(
      perf %>% mutate(Set = "Demographics Only"), 
      breaks = anes_years,
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

# Save summary dataframes ======================================================
cross2(c("pid"), c("logit", "cart", "rf", "ol")) %>%
  map(
    ~ {
      summ_df <- seq(3) %>%
        map(
          function(x) if (!(x == 3 & .x[[2]] == "ol")) {
            perf_summ(perf, .x[[1]], .x[[2]], x)
          }
        ) %>%
        bind_rows(.id = "Y") %>%
        mutate(
          Y = factor(
            Y, levels = seq(3),
            labels = c("7-pt PID", "3-pt PID", "Binary PID")
          )
        )
      
      save(
        summ_df,
        file = here(
          "output", "ANES",
          paste0("perf_summ_ANES_", .x[[1]], "_", .x[[2]], ".Rda")
        )
      )
    }
  )

# Draw time-series performances ================================================
for (method in c("logit", "cart", "ol", "rf")) {
  perf <- loadRData(
    here("output/ANES/", paste0("perf_summ_ANES_pid_", method, ".Rda"))
  ) %>%
    rename(Survey = Y)
  if (method == "ol") {
    temp <- c("7-pt PID", "3-pt PID") %>%
      set_names(., nm = c("pid7", "pid3"))
  } else {
    temp <- c("7-pt PID", "3-pt PID", "Binary PID") %>%
      set_names(., nm = c("pid7", "pid3", "pid2"))
  }
  
  ## TS performance ============================================================
  for (metric in c("Accuracy", "AUC", "prAUC")) {
    pdf(
      here(
        "fig",
        paste0("survey_", method, "_", tolower(metric), "_ts_pid_predict.pdf")
      ),
      width = width, height = (height + 0.5)
    )
    print(
      # https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
      plot_temp() +
        scale_colour_manual(
          values = viridisLite::viridis(3, end = .85),
          name = "Specification"
        ) +
        guides(linetype = guide_legend(title = "Variable"))
    )
    dev.off()
  }

  ## Accuracy ranges for each PID specification ================================
  temp %>%
    map(
      ~ perf %>%
        filter(Survey == .x) %>%
        select(Year, Accuracy_lower, Accuracy_upper, Accuracy, Survey) %>%
        ggplot(aes(x = Year, y = Accuracy, color = Survey, shape = Survey)) +
        geom_pointrange(aes(ymin = Accuracy_lower, ymax = Accuracy_upper)) +
        scale_x_continuous(breaks = anes_years) +
        scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, 0.1)) +
        scale_color_viridis_d(end = 0.85) +
        guides(linetype = guide_legend(title = "Variable"))
    ) %>%
    imap(
      ~ {
        pdf(
          here(paste0("fig/pid_", method, "_accrange_ts_", .y, ".pdf")),
          width = 6.8, height = height
        )
        print(pdf_default(.x))
        dev.off()
      }
    )
  
  ## Average accuracy ==========================================================
  for (metric in c("Accuracy", "AUC", "prAUC")) {
    temp %>%
      imap(
        ~ {
          x <- perf %>% filter(Survey == .x) %>% .$Accuracy %>% mean()
          write(
            round(x * 100, digits = 1),
            file = here(
              "tab", "avg", 
              paste0("ANES", "_avg_", .y, "_", tolower(metric), ".tex")
            )
          )
        }
      )
  }
  
  ## Linear regression =========================================================
  for (metric in c("Accuracy", "AUC", "prAUC")) {
    temp %>%
      imap(
        ~ {
          lm_out <- perf %>% filter(Survey == .x) %>%
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
                        "ANES", "_ts_", .y, "_", tolower(metric),
                        "_slope_", y, ".tex"
                      )
                    )
                  )
              }
            )
        }
      )
  }
}

# Improvement from Baseline ====================================================
## Baseline
anes <- read_dta(here("data/anes/anes_timeseries_cdf.dta")) %>%
  filter(VCF0004 != 1948 & !is.na(VCF0303)) %>%
  mutate(
    pid2 = case_when(
      VCF0303 == 1 ~ VCF0303,
      VCF0303 == 3 ~ 0
    )
  )
anes_2020 <- 
  read_dta(here("data/anes/anes_timeseries_2020_stata_20210719.dta")) %>%
  filter(V201231x %in% seq(7)) %>%
  mutate(
    pid3 = case_when(
      V201231x %in% c(1, 2, 3) ~ 1,
      V201231x == 4 ~ 2,
      V201231x %in% c(5, 6, 7) ~ 3
    )
  ) %>%
  mutate(
    pid2 = case_when(
      pid3 == 1 ~ pid3,
      pid3 == 3 ~ 0
    )
  )

baseline_pid2 <- baseline_pid3 <- baseline_pid7 <- list()
for (year in anes_years) {
  if (year == 2020) {
    temp <- as.numeric(prop(anes_2020, "V201231x")) / 100
  } else {
    temp <- as.numeric(prop(anes %>% filter(VCF0004 == year), "VCF0301")) / 100
  }
  baseline_pid7[[paste0("year", year)]] <- sum(temp * temp)
  
  if (year == 2020) {
    temp <- as.numeric(prop(anes_2020, "pid3")) / 100
  } else {
    temp <- as.numeric(prop(anes %>% filter(VCF0004 == year), "VCF0303")) / 100
  }
  baseline_pid3[[paste0("year", year)]] <- sum(temp * temp)
  
  if (year == 2020) {
    temp <- as.numeric(prop(anes_2020, "pid2", useNA = "no")) / 100
  } else {
    temp <- as.numeric(prop(anes, "pid2", useNA = "no")) / 100
  }
  baseline_pid2[[paste0("year", year)]] <- sum(temp * temp)
}

## Compare the difference
perf <- loadRData(here("output", "ANES", "perf_summ_ANES_pid_rf.Rda")) %>%
  rename(Survey = Y)

temp <- perf %>% filter(Survey == "7-pt PID") %>% .$Accuracy
mean(temp - (baseline_pid7 %>% unlist())) ## 0.09485

temp <- perf %>% filter(Survey == "3-pt PID") %>% .$Accuracy
mean(temp - (baseline_pid3 %>% unlist())) ## 0.1371

temp <- perf %>% filter(Survey == "Binary PID") %>% .$Accuracy
mean(temp - (baseline_pid2 %>% unlist())) ## 0.1183

perf <- loadRData(here("output", "ANES", "perf_summ_ANES_pid_logit.Rda")) %>%
  rename(Survey = Y)

temp <- perf %>% filter(Survey == "7-pt PID") %>% .$Accuracy
mean(temp - (baseline_pid7 %>% unlist())) ## 0.09208

temp <- perf %>% filter(Survey == "3-pt PID") %>% .$Accuracy
mean(temp - (baseline_pid3 %>% unlist())) ## 0.1408

temp <- perf %>% filter(Survey == "Binary PID") %>% .$Accuracy
mean(temp - (baseline_pid2 %>% unlist())) ## 0.1199
