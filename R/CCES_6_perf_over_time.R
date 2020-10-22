source("R/CCES_0_utilities.R")
load("output/CCES/CCES_perf.RData")
load("output/CCES/CCES_varimp.RData")

summ_df <- seq(4) %>%
  map(~ perf_summ(perf, 1, "rf", .x)) %>%
  bind_rows(.id = "Set") %>%
  mutate(
    Set = factor(
      Set, levels = seq(4),
      labels = c(
        "Demographics", "Demo. + PID", "Demo. + PID + Issues",
        "All Observables"
      )
    )
  ) %>%
  filter(Year != 2006) ## House

po_plot <- function(x, metric) {
  p <- ggplot(
    x, aes(
      x = Year, y = !!as.name(metric), group = Set, colour = Set, linetype = Set
    )
  ) +
    geom_line(size = 1) +
    scale_x_continuous(breaks = seq(2008, 2018, by = 2)) +
    scale_color_viridis_d(direction = -1, name = "Variable Specification") +
    scale_linetype_manual(
      name = "Variable Specification",
      values = c("solid", "twodash", "dotted", "dashed")
    ) +
    guides(
      colour = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    ) + 
    scale_y_continuous(limits = c(0.38, 1.0))
  return(p)
}

pdf("fig/CCES/rf/cces_accuracy_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "Accuracy")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) + 
  scale_y_continuous(limits = c(0.5, 1.0))
dev.off()

pdf("fig/CCES/rf/cces_auc_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "AUC")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) + 
  scale_y_continuous(limits = c(0.5, 1.0))
dev.off()

pdf("fig/CCES/rf/cces_prec_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "Precision")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
dev.off()

pdf("fig/CCES/rf/cces_recall_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "Recall")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"))
dev.off()

pdf("fig/CCES/rf/cces_F1_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(po_plot(summ_df, metric = "F1")) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) + 
  scale_y_continuous(limits = c(0.5, 1.0))
dev.off()

## Demographics Accuracy Range
p <- summ_df %>% 
  filter(Set == "Demographics") %>% 
  select(Year, CI, Accuracy) %>%
  rowwise() %>%
  mutate(
    lower = str_match_all(CI, "\\[(.*?),")[[1]][,2],
    upper = str_match_all(CI, " (.*?)\\]")[[1]][,2]
  ) %>%
  select(-CI) %>%
  mutate_if(is.character, as.numeric) %>%
  ggplot(aes(x = Year, y = Accuracy)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) + 
  scale_x_continuous(breaks = seq(2008, 2018, by = 2)) +
  scale_y_continuous(limits = c(0.58, 0.7))

pdf("fig/CCES/rf/cces_demo_acc_range_overtime.pdf", width = 6, height = 4)
Kmisc::pdf_default(p)
dev.off()



