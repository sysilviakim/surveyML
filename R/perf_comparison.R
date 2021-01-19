source("R/utilities.R")

# Import performance summaries =================================================
perf <- list(
  CCNS = loadRData("data/cces-tidy/perf_summ_CCES_Nationscape.Rda") %>%
    mutate(Survey = "CCES/NS"),
  ANES = loadRData("data/anes-tidy/perf_summ_ANES.Rda") %>%
    mutate(Survey = "ANES")
) %>%
  bind_rows()
assert_that(length(unique(perf$Set)) == 4)

#
po_plot <- function(x, metric) {
  ggplot(
    x, aes(x = Year, y = !!as.name(metric), colour = Set, linetype = Survey)
  ) +
    geom_line(size = 1) +
    scale_x_continuous(breaks = seq(1952, 2020, by = 4)) +
    scale_color_viridis_d(
      direction = -1, name = "Specification", end = 0.85
    ) +
    scale_linetype_manual(name = "Survey", values = c("solid", "dashed")) +
    guides(
      colour = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    scale_y_continuous(limits = c(0.38, 1.0))
}

# Export figures ===============================================================
pdf("fig/survey_rf_accuracy_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_plot(perf, metric = "Accuracy")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
    scale_y_continuous(limits = c(0.5, 1.0))
)
dev.off()

pdf("fig/survey_rf_auc_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_plot(perf, metric = "AUC")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
    scale_y_continuous(limits = c(0.5, 1.0))
)
dev.off()

pdf("fig/survey_rf_prec_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_plot(perf, metric = "Precision")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
)
dev.off()

pdf("fig/survey_rf_recall_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_plot(perf, metric = "Recall")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
    scale_y_continuous(limits = c(0.0, 1.0))
)
dev.off()

pdf("fig/survey_rf_F1_ts.pdf", width = 7, height = 4)
print(
  pdf_default(po_plot(perf, metric = "F1")) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
    scale_y_continuous(limits = c(0.0, 1.0))
)
dev.off()

## Demographics Accuracy Range
p <- perf %>%
  rowwise() %>%
  mutate(
    Survey = case_when(
      Survey == "CCES/NS" & Year == 2020 ~ "NS",
      Survey == "CCES/NS" & Year != 2020 ~ "CCES",
      TRUE ~ "ANES"
    )
  ) %>%
  filter(Set == "Demographics Only") %>%
  select(Year, CI, Accuracy, Survey) %>%
  rowwise() %>%
  mutate(
    lower = str_match_all(CI, "\\[(.*?),")[[1]][, 2],
    upper = str_match_all(CI, " (.*?)\\]")[[1]][, 2]
  ) %>%
  select(-CI) %>%
  mutate_at(vars(lower, upper), as.numeric) %>%
  ggplot(aes(x = Year, y = Accuracy, color = Survey)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_continuous(breaks = seq(1952, 2020, by = 4)) +
  scale_y_continuous(limits = c(0.45, 0.8)) +
  scale_color_viridis_d(end = 0.8)

pdf("fig/survey_rf_demo_accrange_ts.pdf", width = 7, height = 4)
print(pdf_default(p))
dev.off()
