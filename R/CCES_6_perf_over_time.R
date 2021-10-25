source(here::here("R", "utilities.R"))
load(here("output/CCES/CCES_perf.RData"))
load(here("output/CCES/CCES_varimp.RData"))

# CCES performance into a table (pres vote choice, 2008-2018) ==================
summ_df <- seq(8) %>%
  map(
    ~ perf_summ(within(perf, rm("year2006")), 1, "rf", .x, yr = rev(cces_years))
  ) %>%
  bind_rows(.id = "Set") %>%
  mutate(Set = factor(Set, levels = seq(8), labels = set_labels)) %>%
  arrange(desc(Year), Set) %>%
  bind_rows(
    ., 
    list(
      readRDS(here("output/NS_perf_RF.RDS")),
      readRDS(here("output/NS_perf_RF_additional.RDS"))
    ) %>%
      bind_rows() %>%
      rename(
        Set = `Model and Specification`,
        Accuracy_lower = AccuracyLower,
        Accuracy_upper = AccuracyUpper
      ) %>%
      mutate(
        Set = gsub("RF: ", "", Set), 
        Set = gsub("Symbolic ideology", "Ideology", Set),
        Year = 2020
      )
  )
save(summ_df, file = here("output/CCES/perf_summ_CCES_Nationscape.Rda"))

# CCES performance but logit ===================================================
summ_df <- seq(8) %>%
  map(
    ~ perf_summ(
      within(perf, rm("year2006")), 1, "logit", .x, yr = rev(cces_years)
    )
  ) %>%
  bind_rows(.id = "Set") %>%
  mutate(Set = factor(Set, levels = seq(8), labels = set_labels)) %>%
  arrange(desc(Year), Set) %>%
  bind_rows(
    ., readRDS(here("output/NS_perf_Logit.RDS")) %>%
      rename(
        Set = `Model and Specification`,
        Accuracy_lower = AccuracyLower,
        Accuracy_upper = AccuracyUpper
      ) %>%
      mutate(
        Set = gsub("RF: ", "", Set), 
        Set = gsub("Symbolic ideology", "Ideology", Set),
        Year = 2020
      )
  )
save(summ_df, file = here("output/CCES/perf_summ_CCES_Nationscape_Logit.Rda"))

# SI figures ===================================================================
cross2(c(preschoice = 1, house = 3, senate = 4), seq(4)) %>%
  map(setNames, c("yvar", "set")) %>%
  map(
    function(x) {
      pdf(
        here(
          "fig", "CCES",
          paste0(
            "CCES_perf_",
            case_when(
              x$yvar == 1 ~ "prezvote",
              x$yvar == 3 ~ "house",
              TRUE ~ "senate"
            ),
            "_set", x$set, ".pdf"
          )
        ),
        width = 6, height = 1.5
      )
      grid_arrange_shared_legend(
        list = roc_comparison(
          within(perf, rm("year2006")),
          yvar = x$yvar, set = x$set
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
        ncol = 6, nrow = 1
      )
      dev.off()
    }
  )
