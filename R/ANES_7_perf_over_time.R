source(here::here("R", "utilities.R"))
load(here("output/ANES/ANES_perf.Rda"))
load(here("output/ANES/ANES_varimp.Rda"))
load(here("output", "summ_list.Rda"))

# RF (Fig 1A) ==================================================================
perf_df <- summ_df$rf %>%
  filter(`Variable Specification` == "Demographics Only") %>%
  mutate(Set = "Vote Choice")

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
