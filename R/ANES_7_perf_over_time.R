source(here::here("R", "utilities.R"))
load(here("output", "summ_list.Rda"))
load(here("output", "summ_list_pid.Rda"))
methods <- c("logit", "cart", "rf") %>% set_names(., .)

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

# Fig 1A and 1B ================================================================
methods %>%
  map(
    ~ {
      p <- po_full(
        perf_list[[.x]], "Accuracy",
        ylim = c(0, 1), name = "Outcome Variable", y2 = TRUE,
        colour_nrow = 1, linetype_nrow = 1, end = 0.85, accrange = TRUE
      )
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


# Logit (Fig 1B) ===============================================================

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
