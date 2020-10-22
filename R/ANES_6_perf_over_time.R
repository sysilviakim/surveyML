source("R/CCES_0_utilities.R")
load("output/ANES/ANES_perf.RData")
load("output/ANES/ANES_varimp.RData")

summ_df <- seq(4) %>%
  map(~ perf_summ(perf, 1, "rf", .x, yr = seq(1948, 2016, 4))) %>%
  bind_rows(.id = "Set") %>%
  mutate(
    Set = factor(
      Set, levels = seq(4),
      labels = c(
        "Demographics", "Demo. + PID", "Demo. + PID + Issues",
        "All Observables"
      )
    )
  )

po_plot <- function(x, metric) {
  p <- ggplot(
    x, aes(
      x = Year, y = !!as.name(metric), group = Set, colour = Set, linetype = Set
    )
  ) +
    geom_line(size = 1) +
    scale_x_continuous(breaks = seq(1948, 2016, by = 4)) +
    scale_color_viridis_d(direction = -1) + # name = "Variable Specification"
    # scale_linetype_manual(
    #   name = "Variable Specification",
    #   values = c("solid", "twodash", "dotted", "dashed")
    # ) +
    guides(
      colour = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )  
    # scale_y_continuous(limits = c(0.38, 1.0))
  return(p)
}

pdf("fig/ANES/rf/ANES_accuracy_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "Accuracy")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
dev.off()

pdf("fig/ANES/rf/ANES_auc_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "AUC")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
dev.off()

pdf("fig/ANES/rf/ANES_prec_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "Precision")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
dev.off()

pdf("fig/ANES/rf/ANES_recall_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "Recall")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
dev.off()

pdf("fig/ANES/rf/ANES_F1_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "F1")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
dev.off()

