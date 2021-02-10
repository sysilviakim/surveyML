source(here("R", "utilities.R"))

load(here("output/CCES/CCES_perf.RData"))
load(here("output/CCES/CCES_varimp.RData"))
pv <- c("pid7", "V212d", "CC307a", "v3007")
ev <- c("educ", "V213")

# vote choice varimp intersection: tables ======================================

# Do demographics linger in top n?
# No, nothing until top 30
vi_inter_btw(
  vid, y = 1, from = 1, to = 4, top = 20, pid_vec = pv, educ_vec = ev
)

tab <- vi_inter_btw(
  within(vid, rm("year2006")),
  y = 1, from = 1, to = 2, top = 10
) %>%
  xtable(
    label = "tab:CCES_vi_btw_sets_demo_1",
    caption = NULL
  )
print(
  tab,
  file = here("tab/CCES_rf_vi_btw_sets_demo_1.tex"), floating = FALSE,
  digits = 0, include.rownames = FALSE, booktabs = TRUE
)

tab <- vi_inter_btw(
  within(vid, rm("year2006")),
  y = 1, from = 1, to = 3, top = 10
) %>%
  xtable(
    label = "tab:CCES_vi_btw_sets_demo_2",
    caption = NULL
  )
print(
  tab,
  file = here("tab/CCES_rf_vi_btw_sets_demo_2.tex"), floating = FALSE,
  digits = 0, include.rownames = FALSE, booktabs = TRUE
)

## Between years within-set? Again, lots of manual work
vi_inter_within(set = 1, top = 5, vid) ## Black, age, gender
vi_inter_within(set = 2, top = 5, vid) ## Dominated by "Strong Republican"
vi_inter_within(set = 3, top = 5, vid)

# Importance of race over the years ============================================

## Restricted to set 2, not 4!
cap <- "Base category: Strong Democrat"
p_list <- list(
  p1 = vi_ts_demo(vid, set = 2) + scale_y_continuous(limits = c(0, 10)),
  p2 = vi_ts_edu(vid, set = 2) + scale_y_continuous(limits = c(0, 10)),
  p3 = vi_ts_pid(vid, set = 2) +
    labs(caption = cap) +
    scale_y_continuous(limits = c(0, 100)),
  p4 = vi_ts_pid(vid, set = 3) +
    labs(caption = cap) +
    scale_y_continuous(limits = c(0, 100)),
  p5 = vi_ts_pid(vid, set = 4) +
    labs(caption = cap) +
    scale_y_continuous(limits = c(0, 100))
)

# Export =======================================================================
pdf(here("fig/CCES/rf/CCES_varimp_ts_set2_demo.pdf"), width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p1), nrow = 3))
dev.off()
pdf(here("fig/CCES/rf/CCES_varimp_ts_set2_edu.pdf"), width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p2), nrow = 3))
dev.off()

pdf(here("fig/CCES/rf/CCES_varimp_ts_set2_pid.pdf"), width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p3), nrow = 3))
dev.off()
pdf(here("fig/CCES/rf/CCES_varimp_ts_set3_pid.pdf"), width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p4), nrow = 3))
dev.off()
pdf(here("fig/CCES/rf/CCES_varimp_ts_set4_pid.pdf"), width = 4, height = 3.8)
print(vi_bottom(pdf_default(p_list$p5), nrow = 3))
dev.off()
