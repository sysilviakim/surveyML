source(here::here("R", "utilities.R"))
perf_cces <- loadRData(here("output/CCES/CCES_perf.RData"))
perf_anes <- loadRData(here("output/ANES/ANES_perf.RData"))

# ANES/CCES/NS but this time, Logit and CART ===================================
perf <- c(Logit = "Logit", CART = "CART") %>%
  imap_dfr(
    function(x, y) {
      seq(4) %>%
        map(
          ~ bind_rows(
            perf_summ(perf_anes, 1, tolower(x), .x, yr = rev(anes_years)) %>%
              mutate(Survey = "ANES"),
            perf_summ(
              within(perf_cces, rm("year2006")),
              1, tolower(x), .x,
              yr = rev(cces_years)
            ) %>%
              mutate(Survey = "CCES")
          )
        ) %>%
        bind_rows(.id = "Set") %>%
        mutate(
          Set = factor(Set, levels = seq(4), labels = set_labels[seq(4)])
        ) %>%
        bind_rows(
          .,
          readRDS(here(paste0("output/NS_perf_", x, ".RDS"))) %>%
            rename(
              Set = `Model and Specification`,
              Accuracy_lower = AccuracyLower,
              Accuracy_upper = AccuracyUpper
            ) %>%
            mutate(
              Set = gsub("RF: |CART: |Logit: ", "", Set),
              Set = gsub("Symbolic ideology", "Ideology", Set),
              Year = 2020,
              Survey = "NS"
            )
        )
    },
    .id = "Algorithm"
  ) %>%
  mutate(Set = factor(Set, levels = set_labels[seq(4)])) %>%
  arrange(Algorithm, desc(Year), Set) %>%
  filter(!is.na(Set))

# If put into simple linear regression =========================================
for (algorithm in c("Logit", "CART")) {
  for (metric in c("Accuracy", "AUC", "F1")) {
    cross2(
      c("ANES", "CCES", "NS", "all"), 
      setNames(set_labels[seq(4)], nm = paste0("set", seq(4)))
    ) %>%
      map(
        ~ {
          if (.x[[1]] == "all") {
            temp <- perf %>% filter(Algorithm == algorithm)
          } else {
            temp <- perf %>% filter(Algorithm == algorithm & Survey == .x[[1]])
          }
          temp <- temp %>%
            filter(Set == .x[[2]])
          
          # First, average of metric =============================================
          write(
            round(mean(temp[[metric]]) * 100, digits = 1),
            file = here(
              "tab", "avg",
              paste0(
                .x[[1]], "_avg_", paste0("set", which(set_labels == .x[[2]])),
                "_", tolower(metric), "_", algorithm, ".tex"
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
                          paste0("set", which(set_labels == .x[[2]])),
                          "_", tolower(metric),
                          "_slope_", y, "_", algorithm, ".tex"
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

# Settings =====================================================================
width <- 6.2
height <- 4
plot_temp <- function(perf, sets = seq(4), ...) {
  p <- pdf_default(
    po_full(
      perf %>% filter(Set %in% levels(perf$Set)[sets]),
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

# Export figures ===============================================================
for (alg in c("Logit", "CART")) {
  for (metric in c("Accuracy", "AUC", "Precision", "Recall", "F1")) {
    pdf(
      here("fig", paste0("survey_", alg, "_", tolower(metric), "_ts.pdf")),
      width = width, height = height
    )
    print(plot_temp(perf %>% filter(Algorithm == alg)))
    dev.off()
  }
}

for (alg in c("Logit", "CART")) {
  levels(perf$Set) %>%
    set_names(., nm = paste0("spec", seq(4))) %>%
    map(
      ~ perf %>%
        filter(Algorithm == alg & Set == .x) %>%
        select(Year, Accuracy_lower, Accuracy_upper, Accuracy, Survey) %>%
        ggplot(aes(x = Year, y = Accuracy, color = Survey, shape = Survey)) +
        geom_pointrange(aes(ymin = Accuracy_lower, ymax = Accuracy_upper)) +
        scale_x_continuous(breaks = c(anes_years, 2020)) +
        scale_y_continuous(limits = c(.4, 1.0), breaks = seq(0.5, 1.0, 0.1)) +
        scale_color_viridis_d(end = 0.85)
    ) %>%
    imap(
      ~ {
        pdf(
          here(paste0("fig/survey_", alg, "_accrange_ts_", .y, ".pdf")),
          width = 6.8, height = height
        )
        print(pdf_default(.x))
        dev.off()
      }
    )
}
