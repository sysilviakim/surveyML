source("R/utilities.R")
load("output/ANES/ANES_perf.RData")
load("output/ANES/ANES_varimp.RData")

# Varimp over time fxn =========================================================
vi_ts_demo <- function(x, y = 1, set = 4, method = "rf", names = "Demographics",
                       yrs = seq(1952, 2016, by = 4)) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(grepl("vcf0104_2", rownames)) %>% select(Gender = Overall),
        .x %>%
          filter(grepl("vcf0101", rownames)) %>% select(Age = Overall),
        .x %>%
          filter(grepl("vcf0105b_2", rownames)) %>% select(Black = Overall),
        ## For some years, race beyond white and black not there
        .x %>%
          filter(grepl("vcf0105b_3", rownames)) %>%
          select(Hispanic = Overall) %>%
          bind_rows(., data.frame(Hispanic = NA)) %>%
          slice(1)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs)
}

vi_ts_edu <- function(x, y = 1, set = 4, method = "rf", 
                      names = "Demographics", yrs = seq(1952, 2016, by = 4),
                      lvl = c(
                        "HS Graduate", "Some College", "College+"
                      )) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(grepl("vcf0110_2", rownames)) %>%
          select(`HS Graduate` = Overall),
        .x %>%
          filter(grepl("vcf0110_3", rownames)) %>%
          select(`Some College` = Overall),
        .x %>%
          filter(grepl("vcf0110_4", rownames)) %>%
          select(`College+` = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs, lvl = lvl)
}

vi_ts_pid <- function(x, y = 1, set = 4, names = "Party ID",
                      yrs = seq(1952, 2016, by = 4),
                      lvl = rev(c(
                        "Weak Democrat", "Lean Democrat", "Independent",
                        "Lean Republican", "Weak Republican",
                        "Strong Republican"
                      ))) {
  x %>%
    map(y) %>%
    map("rf") %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(grepl("vcf0301_2", rownames)) %>%
          select(`Weak Democrat` = Overall),
        .x %>%
          filter(grepl("vcf0301_3", rownames)) %>%
          select(`Lean Democrat` = Overall),
        .x %>%
          filter(grepl("vcf0301_4", rownames)) %>%
          select(`Independent` = Overall),
        .x %>%
          filter(grepl("vcf0301_5", rownames)) %>%
          select(`Lean Republican` = Overall),
        .x %>%
          filter(grepl("vcf0301_6", rownames)) %>%
          select(`Weak Republican` = Overall),
        .x %>%
          filter(grepl("vcf0301_7", rownames)) %>%
          select(`Strong Republican` = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs, lvl = lvl)
}

# Importance of demo/pid over the years ========================================
## Restricted to set 2, not 4!
cap <- "Base category: Strong Democrat"
p_list <- list(
  p1 = vi_ts_demo(vid, set = 2) + scale_y_continuous(limits = c(0, 70)),
  p2 = vi_ts_edu(vid, set = 2) + scale_y_continuous(limits = c(0, 70)),
  p3 = vi_ts_demo(vid, set = 3),
  p4 = vi_ts_edu(vid, set = 3),
  p5 = vi_ts_demo(vid, set = 4),
  p6 = vi_ts_edu(vid, set = 4),
  p7 = vi_ts_pid(vid, set = 2) + 
    labs(caption = cap) + 
    scale_y_continuous(limits = c(0, 100)),
  p8 = vi_ts_pid(vid, set = 3) + 
    labs(caption = cap) + 
    scale_y_continuous(limits = c(0, 100)),
  p9 = vi_ts_pid(vid, set = 4) + 
    labs(caption = cap) + 
    scale_y_continuous(limits = c(0, 100))
)

# Export =======================================================================
pdf("fig/ANES/rf/ANES_varimp_ts_set2_demo.pdf", width = 6, height = 4)
vi_bottom(Kmisc::pdf_default(p_list$p1), nrow = 1)
dev.off()
pdf("fig/ANES/rf/ANES_varimp_ts_set2_edu.pdf", width = 6, height = 4)
vi_bottom(Kmisc::pdf_default(p_list$p2), nrow = 1)
dev.off()

pdf("fig/ANES/rf/ANES_varimp_ts_set2_pid.pdf", width = 6, height = 4)
vi_bottom(Kmisc::pdf_default(p_list$p7))
dev.off()
pdf("fig/ANES/rf/ANES_varimp_ts_set3_pid.pdf", width = 6, height = 4)
vi_bottom(Kmisc::pdf_default(p_list$p8))
dev.off()
pdf("fig/ANES/rf/ANES_varimp_ts_set4_pid.pdf", width = 6, height = 4)
vi_bottom(Kmisc::pdf_default(p_list$p9))
dev.off()
