source("R/utilities.R")
load("output/ANES/ANES_perf.RData")
load("output/ANES/ANES_varimp.RData")

# vote choice varimp intersection: tables ======================================

tab <- vi_inter_btw(
  vid, y = 1, from = 1, to = 2, top = 10, 
  pid_vec = "vcf0301", educ_vec = "vcf0110", survey = "ANES"
) %>%
  xtable(label = "tab:ANES_vi_btw_sets_demo_1", caption = NULL)
print(
  tab,
  file = "tab/ANES_rf_vi_btw_sets_demo_1.tex", floating = FALSE,
  digits = 0, include.rownames = FALSE, booktabs = TRUE
)

tab <- vi_inter_btw(
  vid, y = 1, from = 1, to = 3, top = 10, 
  pid_vec = "vcf0301", educ_vec = "vcf0110", survey = "ANES"
) %>%
  xtable(label = "tab:ANES_vi_btw_sets_demo_2", caption = NULL)
print(
  tab,
  file = "tab/ANES_rf_vi_btw_sets_demo_2.tex", floating = FALSE,
  digits = 0, include.rownames = FALSE, booktabs = TRUE
)

# Importance of demo/pid over the years ========================================

cap <- "Base category: Strong Democrat"
p_list <- list(
  p1 = vi_ts_demo2(vid, set = 2) + scale_y_continuous(limits = c(0, 70)),
  p2 = vi_ts_edu2(vid, set = 2) + scale_y_continuous(limits = c(0, 70)),
  p3 = vi_ts_pid2(vid, set = 2) + 
    labs(caption = cap) + 
    scale_y_continuous(limits = c(0, 100)),
  p4 = vi_ts_pid2(vid, set = 3) + 
    labs(caption = cap) + 
    scale_y_continuous(limits = c(0, 100)),
  p5 = vi_ts_pid2(vid, set = 4) + 
    labs(caption = cap) + 
    scale_y_continuous(limits = c(0, 100))
) %>%
  map(
    ~ .x + 
      scale_x_continuous(
        breaks = seq(1952, 2016, by = 4),
        labels = function(x) paste0("\'", str_sub(x, 3, 4))
      )
  )

# Export =======================================================================
pdf("fig/ANES/rf/ANES_varimp_ts_set2_demo.pdf", width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p1), nrow = 3))
dev.off()
pdf("fig/ANES/rf/ANES_varimp_ts_set2_edu.pdf", width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p2), nrow = 3))
dev.off()

pdf("fig/ANES/rf/ANES_varimp_ts_set2_pid.pdf", width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p3), nrow = 3))
dev.off()
pdf("fig/ANES/rf/ANES_varimp_ts_set3_pid.pdf", width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p4), nrow = 3))
dev.off()
pdf("fig/ANES/rf/ANES_varimp_ts_set4_pid.pdf", width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p5), nrow = 3))
dev.off()
