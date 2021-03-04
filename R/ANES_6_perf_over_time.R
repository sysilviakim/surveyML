source(here::here("R", "utilities.R"))
load(here("output/ANES/ANES_perf.RData"))
load(here("output/ANES/ANES_varimp.RData"))
anes_years <- seq(1952, 2016, by = 4)

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
p <- seq(4) %>%
  set_names(., paste0("p", .)) %>%
  map(~ roc_comparison(perf, set = .x))

seq(4) %>%
  map(
    function(x) {
      pdf(
        here("fig", "ANES", paste0("ANES_perf_prezvote_set", x, ".pdf")),
        width = 6, height = 5
      )
      grid_arrange_shared_legend(
        list = p[[paste0("p", x)]] %>%
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
