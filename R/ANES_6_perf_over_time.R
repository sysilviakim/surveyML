source("R/utilities.R")
load("output/ANES/ANES_perf.RData")
load("output/ANES/ANES_varimp.RData")
anes_years <- seq(1952, 2016, by = 4)

summ_df <- seq(4) %>%
  map(~ perf_summ(perf, 1, "rf", .x, yr = seq(1952, 2016, 4))) %>%
  bind_rows(.id = "Set") %>%
  mutate(Set = factor(Set, levels = seq(4), labels = set_labels))

save(summ_df, file = "data/anes-tidy/perf_summ_ANES.Rda")

# Export figures ===============================================================
pdf("fig/ANES/rf/ANES_accuracy_overtime.pdf", width = 6, height = 4)
print(
  pdf_default(
    po_plot(summ_df, metric = "Accuracy", years = anes_years, ylim = NULL)
  ) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
)
dev.off()

pdf("fig/ANES/rf/ANES_auc_overtime.pdf", width = 6, height = 4)
print(
  pdf_default(
    po_plot(summ_df, metric = "AUC", years = anes_years, ylim = NULL)
  ) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
)
dev.off()

pdf("fig/ANES/rf/ANES_prec_overtime.pdf", width = 6, height = 4)
print(
  pdf_default(
    po_plot(summ_df, metric = "Precision", years = anes_years, ylim = NULL)
  ) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
)
dev.off()

pdf("fig/ANES/rf/ANES_recall_overtime.pdf", width = 6, height = 4)
print(
  pdf_default(
    po_plot(summ_df, metric = "Recall", years = anes_years, ylim = NULL)
  ) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
)
dev.off()

pdf("fig/ANES/rf/ANES_F1_overtime.pdf", width = 6, height = 4)
print(
  pdf_default(
    po_plot(summ_df, metric = "F1", years = anes_years, ylim = NULL)
  ) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
)
dev.off()
