source("R/utilities.R")
load("output/CCES/CCES_perf.RData")
load("output/CCES/CCES_varimp.RData")
pid_vec <- c("pid7", "V212d", "CC307a", "v3007")
educ_vec <- c("educ", "V213")

# Within-year, between-set intersection fxn ====================================
vi_inter_btw <- function(x, y = 1, from = 1, to = 4, top = 20) {
  x %>%
    map(y) %>%
    map("rf") %>%
    map(
      ~ Reduce(
        intersect,
        .x %>%
          map(
            function(x) {
              x %>%
                arrange(desc(Overall)) %>%
                slice_head(n = top) %>%
                .$rownames
            }
          ) %>%
          .[from:to]
      )
    ) %>%
    map(
      ~ tibble(x := .x) %>%
        t() %>%
        as_tibble(.name_repair = "unique")
    ) %>%
    bind_rows(.id = "Year") %>%
    mutate(Year = gsub("year", "", Year)) %>%
    mutate_all(~ if_else(. %in% c("gender.2", "V208.2"), "Gender", .)) %>%
    mutate_all(~ if_else(. %in% c("birthyr", "V207", "v207"), "Age", .)) %>%
    mutate_all(
      ~ if_else(. %in% c("race.2", "V211.2", "v211_black"), "Black", .)
    ) %>%
    mutate_all(~ if_else(. %in% c("race.3", "V211.3"), "Hispanic", .)) %>%
    mutate_all(
      ~ if_else(
        . %in% paste0(pid_vec, ".2"), "Not very strong Democrat",
        if_else(
          . %in% paste0(pid_vec, ".3"), "Lean Democrat",
          if_else(
            . %in% paste0(pid_vec, ".4"), "Independent",
            if_else(
              . %in% paste0(pid_vec, ".5"), "Lean Republican",
              if_else(
                . %in% paste0(pid_vec, ".6"), "Not very strong Republican",
                if_else(
                  . %in% paste0(pid_vec, ".7"), "Strong Republican", .
                )
              )
            )
          )
        )
      )
    ) %>%
    mutate_all(
      ~ if_else(
        . %in% paste0(educ_vec, ".2"), "High school graduate",
        if_else(
          . %in% c(paste0(educ_vec, ".3"), "v213_some_college"), "Some college",
          if_else(
            . %in% c(paste0(educ_vec, ".4"), "v213_2_year"), "2-year college",
            if_else(
              . %in% c(paste0(educ_vec, ".5"), "v213_4_year"), "4-year college",
              if_else(
                . %in% paste0(educ_vec, ".6"), "Post-grad", .
              )
            )
          )
        )
      )
    )
}

# Between-year, within-set intersection fxn ====================================
vi_inter_within <- function(x, y = 1, set = 4, top = 20) {
  x %>%
    map(y) %>%
    map("rf") %>%
    map(set) %>%
    map(
      ~ .x %>%
        arrange(desc(Overall)) %>%
        slice_head(n = top) %>%
        .$rownames
    )
  ## Much more manual tagging, I'm afraid.
}

# vote choice varimp intersection: tables ======================================
## Do demographics linger in top n?
vi_inter_btw(vid, y = 1, from = 1, to = 4, top = 20) ## Nothing until top 30
tab <- vi_inter_btw(
  within(vid, rm("year2006")),
  y = 1, from = 1, to = 2, top = 10
) %>%
  xtable(
    label = "tab:CCES_vi_btw_sets_demo_1",
    caption = "Demographics Remaining Within Top 10 Variables, PID Included"
  )
print(
  tab,
  digits = 0, include.rownames = FALSE, booktabs = TRUE
)

tab <- vi_inter_btw(
  within(vid, rm("year2006")),
  y = 1, from = 1, to = 3, top = 10
) %>%
  xtable(
    label = "tab:CCES_vi_btw_sets_demo_2",
    caption =
      "Demographics Remaining Within Top 10 Variables, PID/Issues Included"
  )
print(
  tab,
  digits = 0, include.rownames = FALSE, booktabs = TRUE
)

## Between years within-set? Again, lots of manual work
vi_inter_within(set = 1, top = 5, vid) ## Black, age, gender
vi_inter_within(set = 2, top = 5, vid) ## Dominated by "Strong Republican"
vi_inter_within(set = 3, top = 5, vid)

# Varimp over time fxn =========================================================
vi_ts_demo <- function(x, y = 1, set = 4, method = "rf",
                       names = "Demographics", yrs = seq(2008, 2018, by = 2)) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(
            rownames %in% c("gender.2", "v208_female", "V208.2", "v2004.2")
          ) %>%
          select(Gender = Overall),
        .x %>%
          filter(rownames %in% c("birthyr", "V207", "v207")) %>%
          select(Age = Overall),
        .x %>%
          filter(rownames %in% c("race.2", "V211.2", "v211_black")) %>%
          select(Black = Overall),
        .x %>%
          filter(rownames %in% c("race.3", "V211.3", "v211_hispanic")) %>%
          select(Hispanic = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs)
}

vi_ts_edu <- function(x, y = 1, set = 4, method = "rf",
                      names = "Demographics", yrs = seq(2008, 2018, by = 2),
                      lvl = c(
                        "HS Graduate", "Some College",
                        "2-year", "4-year", "Post-grad"
                      )) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(
            rownames %in% c("educ.2", "v213_high_school_graduate", "V213.2")
          ) %>%
          select(`HS Graduate` = Overall),
        .x %>%
          filter(rownames %in% c("educ.3", "v213_some_college", "V213.3")) %>%
          select(`Some College` = Overall),
        .x %>%
          filter(rownames %in% c("educ.4", "v213_2_year", "V213.4")) %>%
          select(`2-year` = Overall),
        .x %>%
          filter(rownames %in% c("educ.5", "v213_4_year", "V213.5")) %>%
          select(`4-year` = Overall),
        .x %>%
          filter(rownames %in% c("educ.6", "v213_post_grad", "V213.6")) %>%
          select(`Post-grad` = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs, lvl = lvl)
}

vi_ts_pid <- function(x, y = 1, set = 4, method = "rf", names = "Party ID",
                      yrs = seq(2008, 2018, by = 2),
                      lvl = rev(c(
                        "Weak Democrat", "Lean Democrat", "Independent",
                        "Lean Republican", "Weak Republican",
                        "Strong Republican"
                      ))) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(
            rownames %in% c(
              "pid7.2", "v212d_not_very_strong_democrat", "CC307a.2"
            )
          ) %>%
          select(`Weak Democrat` = Overall), ## Not very strong Democrat
        .x %>%
          filter(
            rownames %in% c("pid7.3", "v212d_lean_democrat", "CC307a.3")
          ) %>%
          select(`Lean Democrat` = Overall),
        .x %>%
          filter(
            rownames %in% c("pid7.4", "v212d_independent", "CC307a.4")
          ) %>%
          select(`Independent` = Overall),
        .x %>%
          filter(
            rownames %in% c("pid7.5", "v212d_lean_republican", "CC307a.5")
          ) %>%
          select(`Lean Republican` = Overall),
        .x %>%
          filter(
            rownames %in% c(
              "pid7.6", "v212d_not_very_strong_democrat", "CC307a.6"
            )
          ) %>%
          select(`Weak Republican` = Overall), ## Not very strong Republican
        .x %>%
          filter(
            rownames %in% c("pid7.7", "v212d_strong_democrat", "CC307a.7")
          ) %>%
          select(`Strong Republican` = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs, lvl = lvl)
}

# Importance of race over the years ============================================
## Restricted to set 2, not 4!
cap <- "Base category: Strong Democrat"
p_list <- list(
  p1 = vi_ts_demo(vid, set = 2) + scale_y_continuous(limits = c(0, 10)),
  p2 = vi_ts_edu(vid, set = 2) + scale_y_continuous(limits = c(0, 10)),
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
pdf("fig/CCES/rf/CCES_varimp_ts_set2_demo.pdf", width = 6, height = 4)
vi_bottom(pdf_default(p_list$p1), nrow = 1)
pdf("fig/CCES/rf/CCES_varimp_ts_set2_edu.pdf", width = 6, height = 4)
vi_bottom(pdf_default(p_list$p2), nrow = 1)
dev.off()

pdf("fig/CCES/rf/CCES_varimp_ts_set2_pid.pdf", width = 6, height = 4)
vi_bottom(pdf_default(p_list$p7))
dev.off()
pdf("fig/CCES/rf/CCES_varimp_ts_set3_pid.pdf", width = 6, height = 4)
vi_bottom(pdf_default(p_list$p8))
dev.off()
pdf("fig/CCES/rf/CCES_varimp_ts_set4_pid.pdf", width = 6, height = 4)
vi_bottom(pdf_default(p_list$p9))
dev.off()
