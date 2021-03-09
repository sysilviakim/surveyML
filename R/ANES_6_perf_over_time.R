source(here::here("R", "utilities.R"))
load(here("output/ANES/ANES_perf.RData"))
load(here("output/ANES/ANES_varimp.RData"))

# Summary Rda files ============================================================
cross2(c("prezvote", "house", "senate"), c("logit", "cart", "rf")) %>%
  map(
    ~ {
      summ_df <- seq(4) %>%
        map(
          function(x) perf_summ(perf, .x[[1]], .x[[2]], x, yr = anes_years)
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

# SI figures ===================================================================
cross2(c("prezvote", "house", "senate"), seq(4)) %>%
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
