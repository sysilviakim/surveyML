source("R/CCES_0_utilities.R")
source("R/NS_extract_performance.R")
load("output/CCES/CCES_perf.RData")
load("output/CCES/CCES_varimp.RData")

# CCES performance into a table (pres vote choice, 2008-2018) ==================
summ_df <- seq(4) %>%
  map(
    ~ perf_summ(
      within(perf, rm("year2006")),
      1, "rf", .x,
      yr = rev(seq(2008, 2018, 2))
    )
  ) %>%
  bind_rows(.id = "Set")

# Append NS outputs (pres vote choice, 2020) per Jan's code source outputs =====
summ_df <- summ_df %>%
  bind_rows(
    .,
    data.frame(
      Set = as.character(seq(4)),
      Year = 2020,
      AUC = AUC_Nationscape,
      Accuracy = Accuracy_Nationscape,
      CI = map2(
        AN$Lower, AN$Upper,
        ~ paste0(
          "[",  str_pad(round(.x, digits = 4), 6, "right", "0"),
          ", ", str_pad(round(.y, digits = 4), 6, "right", "0"), "]"
        )
      ) %>%
        unlist() %>%
        unname(),
      Precision = c(
        S1$byClass[c("Precision")], S2$byClass[c("Precision")],
        S3$byClass[c("Precision")], S4$byClass[c("Precision")]
      ),
      Recall = c(
        S1$byClass[c("Recall")], S2$byClass[c("Recall")],
        S3$byClass[c("Recall")], S4$byClass[c("Recall")]
      ),
      F1 = F1_Nationscape,
      stringsAsFactors = FALSE
    )
  ) %>%
  mutate(Set = factor(Set, levels = seq(4), labels = set_labels)) %>%
  arrange(desc(Year), Set)

save(summ_df, file = "data/cces-tidy/perf_summ_CCES_Nationscape.Rda")

# Export figures ===============================================================
pdf("fig/CCES/rf/CCES_Nationscape_accuracy_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "Accuracy")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
  scale_y_continuous(limits = c(0.5, 1.0))
dev.off()

pdf("fig/CCES/rf/CCES_Nationscape_auc_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "AUC")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
  scale_y_continuous(limits = c(0.5, 1.0))
dev.off()

pdf("fig/CCES/rf/CCES_Nationscape_prec_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "Precision")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
dev.off()

pdf("fig/CCES/rf/CCES_Nationscape_recall_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "Recall")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
dev.off()

pdf("fig/CCES/rf/CCES_Nationscape_F1_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "F1")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
  scale_y_continuous(limits = c(0.5, 1.0))
dev.off()

## Demographics Accuracy Range
p <- summ_df %>%
  filter(Set == "Demographics Only") %>%
  select(Year, CI, Accuracy) %>%
  rowwise() %>%
  mutate(
    lower = str_match_all(CI, "\\[(.*?),")[[1]][, 2],
    upper = str_match_all(CI, " (.*?)\\]")[[1]][, 2]
  ) %>%
  select(-CI) %>%
  mutate_if(is.character, as.numeric) %>%
  ggplot(aes(x = Year, y = Accuracy)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2)) +
  scale_y_continuous(limits = c(0.6, 0.7))

pdf(
  "fig/CCES/rf/CCES_Nationscape_demo_acc_range_overtime.pdf", 
  width = 6, height = 4
)
Kmisc::pdf_default(p)
dev.off()
