source(here::here("R", "utilities.R"))

# Import raw and summarized performance ========================================
raw_list <- list(
  ANES_pf = loadRData(here("output/ANES/ANES_perf.RData")),
  ANES_vi = loadRData(here("output/ANES/ANES_varimp.RData")),
  CCES_pf = loadRData(here("output/CCES/CCES_perf.RData")),
  CCES_vi = loadRData(here("output/CCES/CCES_varimp.RData"))
)

perf <- list(
  CCNS = loadRData(here("data/cces-tidy/perf_summ_CCES_Nationscape.Rda")) %>%
    mutate(Survey = "CCES/NS"),
  ANES = loadRData(here("data/anes-tidy/perf_summ_ANES_prezvote_rf.Rda")) %>%
    mutate(Survey = "ANES")
) %>%
  bind_rows()
assert_that(length(unique(perf$Set)) == 8)

# Export figures ===============================================================
for (metric in c("Accuracy", "AUC", "Precision", "Recall", "F1")) {
  pdf(
    here("fig", paste0("survey_rf_", tolower(metric), "_ts.pdf")),
    width = 7, height = 4
  )
  print(
    pdf_default(
      po_full(perf %>% filter(Set %in% set_labels[seq(4)]), metric = metric)
    ) +
      theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
      scale_y_continuous(limits = c(0.5, 1.0))
  )
  dev.off()
}

for (metric in c("Accuracy", "AUC", "Precision", "Recall", "F1")) {
  pdf(
    here("fig", paste0("survey_rf_", tolower(metric), "_ts_SI.pdf")),
    width = 7, height = 4
  )
  print(
    pdf_default(
      po_full(
        perf %>%
          filter(Set %in% set_labels[seq(5, 8)] & Survey == "ANES"),
        metric = "Accuracy", ylim = c(0, 1)
      )
    ) +
      theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
      scale_y_continuous(limits = c(0, 1.0))
  )
  dev.off()
  
  pdf(
    here("fig", paste0("survey_rf_", tolower(metric), "_ts_SI_1.pdf")),
    width = 7, height = 4
  )
  print(
    pdf_default(
      po_full(
        perf %>%
          filter(Set %in% set_labels[c(1, 5, 6)] & Survey == "ANES"),
        metric = "Accuracy", ylim = c(0, 1), colour_nrow = 1
      )
    ) +
      theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
      scale_y_continuous(limits = c(0, 1.0))
  )
  dev.off()
  
  pdf(
    here("fig", paste0("survey_rf_", tolower(metric), "_ts_SI_2.pdf")),
    width = 7, height = 4
  )
  print(
    pdf_default(
      po_full(
        perf %>%
          filter(Set %in% set_labels[c(2, 3, 4, 7, 8)] & Survey == "ANES"),
        metric = "Accuracy", ylim = c(0, 1), end = 0.95
      )
    ) +
      theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
      scale_y_continuous(limits = c(0, 1.0))
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

levels(perf$Set) %>%
  set_names(., nm = paste0("spec", seq(4))) %>%
  map(
    ~ temp %>%
      filter(Set == .x) %>%
      select(Year, Accuracy_lower, Accuracy_upper, Accuracy, Survey) %>%
      ggplot(aes(x = Year, y = Accuracy, color = Survey)) +
      geom_pointrange(aes(ymin = Accuracy_lower, ymax = Accuracy_upper)) +
      scale_x_continuous(breaks = seq(1952, 2020, by = 4)) +
      scale_y_continuous(limits = c(0.45, 1.0), breaks = seq(0.5, 1.0, 0.1)) +
      scale_color_viridis_d(end = 0.85)
  ) %>%
  imap(
    ~ {
      pdf(
        here(paste0("fig/survey_rf_accrange_ts_", .y, ".pdf")),
        width = 7, height = 4
      )
      print(pdf_default(.x))
      dev.off()
    }
  )

## Similarly, AUC Ranges =======================================================
levels(perf$Set) %>%
  set_names(., nm = paste0("spec", seq(4))) %>%
  map(
    ~ temp %>%
      filter(Set == .x) %>%
      select(Year, AUC_lower, AUC_upper, AUC, Survey) %>%
      ggplot(aes(x = Year, y = AUC, color = Survey)) +
      geom_pointrange(aes(ymin = AUC_lower, ymax = AUC_upper)) +
      scale_x_continuous(breaks = seq(1952, 2020, by = 4)) +
      scale_y_continuous(limits = c(0.4, 1.0), breaks = seq(0.4, 1.0, 0.1)) +
      scale_color_viridis_d(end = 0.85)
  ) %>%
  imap(
    ~ {
      pdf(
        here(paste0("fig/survey_rf_aucrange_ts_", .y, ".pdf")),
        width = 7, height = 4
      )
      print(pdf_default(.x))
      dev.off()
    }
  )
