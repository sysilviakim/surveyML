source(here::here("R", "utilities.R"))
load(here("output/ANES/ANES_perf.RData"))
load(here("output/ANES/ANES_varimp.RData"))

# Summary Rda files ============================================================
cross2(c("prezvote", "house", "senate"), c("logit", "cart", "rf")) %>%
  map(
    ~ {
      summ_df <- seq(8) %>%
        map(
          function(x) perf_summ(perf, .x[[1]], .x[[2]], x, yr = anes_years)
        ) %>%
        bind_rows(.id = "Set") %>%
        mutate(Set = factor(Set, levels = seq(8), labels = set_labels))

      save(
        summ_df,
        file = here(
          "output", "ANES",
          paste0("perf_summ_ANES_", .x[[1]], "_", .x[[2]], ".Rda")
        )
      )
    }
  )

# SI figures ===================================================================
cross2(c("prezvote", "house", "senate"), seq(8)) %>%
  map(setNames, c("yvar", "set")) %>%
  map(
    function(x) {
      pdf(
        here(
          "fig", "ANES", paste0("ANES_perf_", x$yvar, "_set", x$set, ".pdf")
        ),
        width = 6, height = 5
      )
      grid_arrange_shared_legend(
        list = roc_comparison(perf, yvar = x$yvar, set = x$set) %>%
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

# Only ideology ================================================================
cross2(c("prezvote"), c("logit", "rf")) %>%
  map(
    ~ {
      summ_df <- seq(9) %>%
        map(
          function(x) perf_summ(
            within(perf, rm(list = paste0("year", seq(1952, 1968, by = 4)))), 
            .x[[1]], .x[[2]], x, yr = seq(1972, 2016, by = 4)
          )
        ) %>%
        bind_rows(.id = "Set") %>%
        mutate(Set = factor(Set, levels = seq(9), labels = set_labels))
      
      save(
        summ_df,
        file = here(
          "output", "ANES",
          paste0("perf_summ_ANES_", .x[[1]], "_", .x[[2]], ".Rda")
        )
      )
    }
  )

cross2(c("prezvote"), seq(9, 9)) %>%
  map(setNames, c("yvar", "set")) %>%
  map(
    function(x) {
      pdf(
        here(
          "fig", "ANES", paste0("ANES_perf_", x$yvar, "_set", x$set, ".pdf")
        ),
        width = 6, height = 5
      )
      grid_arrange_shared_legend(
        list = roc_comparison(
          within(perf, rm(list = paste0("year", seq(1952, 1968, by = 4)))), 
          yvar = x$yvar, set = x$set,
          levels = c("logit", "rf"), labels = c("Logit", "RF")
        ) %>%
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

width <- 6.2
height <- 4
plot_temp <- function(sets = c(1, 2, 4, 5), ...) {
  p <- pdf_default(
    po_full(
      perf %>% filter(Set %in% levels(perf$Set)[sets]),
      metric = metric, vdir = 1, colour_nrow = 1, linetype_nrow = 1
    )
  ) +
    theme(legend.position = "bottom", legend.key.width = unit(.9, "cm")) +
    scale_y_continuous(limits = c(0.4, 1))
  if (metric != "Accuracy" & metric != "AUC") {
    p <- p + scale_y_continuous(limits = c(0, 1))
  }
  return(p)
}

perf <- loadRData(here("output/ANES/perf_summ_ANES_prezvote_rf.Rda")) %>%
  mutate(Set = fct_relevel(Set, set_labels[c(1, 2, 7, 3, 4, 5, 6, 8, 9)])) %>%
  # ANES does not have 3-pt self-identification of ideology until 1972
  filter(Year >= 1972)

for (metric in c("Accuracy", "AUC", "Precision", "Recall", "F1")) {
  pdf(
    here("fig", paste0("survey_rf_", tolower(metric), "_ts_ideo_vs_dem.pdf")),
    width = width, height = (height + 0.5)
  )
  print(
    # https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
    plot_temp(sets = c(1, 9), colour_nrow = 3, end = 1) +
      scale_colour_manual(
        values = viridisLite::viridis(2, end = .85),
        name = "Specification"
      )
  )
  dev.off()
}


