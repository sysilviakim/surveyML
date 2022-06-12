source(here::here("R", "utilities.R"))
load(here("output", "summ_list.Rda"))
load(here("output", "summ_list_pid.Rda"))
load(here("output/ANES/ANES_perf.Rda"))

# Settings =====================================================================
width <- 6.2
height <- 4
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




