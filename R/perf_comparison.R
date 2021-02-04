source("R/utilities.R")

# Import raw and summarized performance ========================================
raw_list <- list(
  ANES_pf = loadRData("output/ANES/ANES_perf.RData"),
  ANES_vi = loadRData("output/ANES/ANES_varimp.RData"),
  CCES_pf = loadRData("output/CCES/CCES_perf.RData"),
  CCES_vi = loadRData("output/CCES/CCES_varimp.RData")
)

perf <- list(
  CCNS = loadRData("data/cces-tidy/perf_summ_CCES_Nationscape.Rda") %>%
    mutate(Survey = "CCES/NS"),
  ANES = loadRData("data/anes-tidy/perf_summ_ANES.Rda") %>%
    mutate(Survey = "ANES")
) %>%
  bind_rows()
assert_that(length(unique(perf$Set)) == 4)

# Export figures ===============================================================
pdf("fig/survey_rf_accuracy_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_full(perf, metric = "Accuracy")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
    scale_y_continuous(limits = c(0.5, 1.0))
)
dev.off()

pdf("fig/survey_rf_auc_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_full(perf, metric = "AUC")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
    scale_y_continuous(limits = c(0.5, 1.0))
)
dev.off()

pdf("fig/survey_rf_prec_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_full(perf, metric = "Precision")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
)
dev.off()

pdf("fig/survey_rf_recall_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_full(perf, metric = "Recall")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
    scale_y_continuous(limits = c(0.0, 1.0))
)
dev.off()

pdf("fig/survey_rf_F1_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_full(perf, metric = "F1")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
    scale_y_continuous(limits = c(0.0, 1.0))
)
dev.off()

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
        paste0("fig/survey_rf_accrange_ts_", .y, ".pdf"), 
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
        paste0("fig/survey_rf_aucrange_ts_", .y, ".pdf"), 
        width = 7, height = 4
      )
      print(pdf_default(.x))
      dev.off()
    }
  )

# If put into simple linear regression =========================================
temp <- perf %>% 
  filter(Survey == "ANES") %>%
  lm(Accuracy ~ Year, data = .) %>% 
  broom::tidy()

temp %>% 
  filter(term == "Year") %>%
  .$estimate %>%
  write(., file = "tab/anes_ts_slope_est.tex")

temp %>% 
  filter(term == "Year") %>%
  .$std.error %>%
  write(., file = "tab/anes_ts_slope_se.tex")

