source(here::here("R", "utilities.R"))

# Import raw and summarized performance ========================================
raw_list <- list(
  ANES_pf = loadRData(here("output/ANES/ANES_perf.RData")),
  ANES_vi = loadRData(here("output/ANES/ANES_varimp.RData")),
  CCES_pf = loadRData(here("output/CCES/CCES_perf.RData")),
  CCES_vi = loadRData(here("output/CCES/CCES_varimp.RData"))
)

perf <- list(
  CCNS = loadRData(here("output/CCES/perf_summ_CCES_Nationscape.Rda")) %>%
    mutate(Survey = "CCES/NS"),
  ANES = loadRData(here("output/ANES/perf_summ_ANES_prezvote_rf.Rda")) %>%
    mutate(Survey = "ANES")
) %>%
  bind_rows() %>%
  mutate(Set = fct_relevel(Set, set_labels[c(1, 2, 7, 3, 4, 5, 6, 8)])) %>%
  # ANES does not have 3-pt self-identification of ideology until 1972
  filter(!(Set == "Demo. + Ideology" & Year < 1972 & Survey == "ANES"))
assert_that(length(unique(perf$Set)) == 8)

# Settings =====================================================================
width <- 6.2
height <- 4
plot_temp <- function(sets = seq(4), ...) {
  p <- pdf_default(
    po_full(
      perf %>% filter(Set %in% set_labels[sets]),
      metric = metric, vdir = 1, ...
    )
  ) +
    theme(legend.position = "bottom", legend.key.width = unit(.9, "cm")) +
    scale_y_continuous(limits = c(0.5, 1))
  if (metric != "Accuracy" & metric != "AUC") {
    p <- p + scale_y_continuous(limits = c(0, 1))
  }
  return(p)
}

# Export figures ===============================================================
for (metric in c("Accuracy", "AUC", "Precision", "Recall", "F1")) {
  pdf(
    here("fig", paste0("survey_rf_", tolower(metric), "_ts.pdf")),
    width = width, height = height
  )
  print(plot_temp())
  dev.off()
}

# Ideology + SI figures ========================================================
for (metric in c("Accuracy", "AUC", "Precision", "Recall", "F1")) {
  pdf(
    here("fig", paste0("survey_rf_", tolower(metric), "_ts_extra.pdf")),
    width = width, height = (height + 0.5)
  )
  print(
    # https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
    plot_temp(sets = c(2, 7, 8), colour_nrow = 3, end = 1) +
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
    here("fig", paste0("survey_rf_", tolower(metric), "_ts_SI_1.pdf")),
    width = width, height = height
  )
  print(
    plot_temp(sets = c(1, 5, 6), colour_nrow = 3, end = 1) +
      scale_colour_manual(
        values = c(
          # Force as the third scale in the default figure
          viridisLite::viridis(9, end = 1)[1],
          viridisLite::viridis(9, end = 1)[8],
          viridisLite::viridis(9, end = 1)[5]
        ), 
        name = "Specification"
      )
    
  )
  dev.off()
}

## Accuracy Range By Specification =============================================
temp <- perf %>%
  rowwise() %>%
  mutate(
    Survey = case_when(
      Survey == "CCES/NS" & Year == 2020 ~ "NS",
      Survey == "CCES/NS" & Year != 2020 ~ "CCES",
      TRUE ~ "ANES"
    )
  )

levels(temp$Set) %>%
  set_names(., nm = paste0("spec", seq(8))) %>%
  map(
    ~ temp %>%
      filter(Set == .x) %>%
      select(Year, Accuracy_lower, Accuracy_upper, Accuracy, Survey) %>%
      ggplot(aes(x = Year, y = Accuracy, color = Survey, shape = Survey)) +
      geom_pointrange(aes(ymin = Accuracy_lower, ymax = Accuracy_upper)) +
      scale_x_continuous(breaks = c(anes_years, 2020)) +
      scale_y_continuous(limits = c(0.45, 1.0), breaks = seq(0.5, 1.0, 0.1)) +
      scale_color_viridis_d(end = 0.85)
  ) %>%
  imap(
    ~ {
      pdf(
        here(paste0("fig/survey_rf_accrange_ts_", .y, ".pdf")),
        width = 6.8, height = height
      )
      print(pdf_default(.x))
      dev.off()
    }
  )

p <- temp %>%
  filter(Set %in% levels(Set)[c(2, 3, 8)]) %>%
  select(Year, Accuracy_lower, Accuracy_upper, Accuracy, Survey, Set) %>%
  ggplot(
    aes(x = Year, y = Accuracy, color = Set, shape = Survey, linetype = Survey)
  ) +
  geom_pointrange(aes(ymin = Accuracy_lower, ymax = Accuracy_upper)) +
  geom_line(aes(x = Year, y = Accuracy)) + 
  scale_x_continuous(breaks = c(anes_years, 2020)) +
  scale_y_continuous(limits = c(0.45, 1.0), breaks = seq(0.5, 1.0, 0.1)) +
  scale_color_viridis_d(end = 0.85, name = "Set")
print(
  pdf_default(p) +
    theme(legend.position = "bottom", legend.key.width = unit(.9, "cm")) +
    scale_y_continuous(limits = c(0.5, 1))
)
