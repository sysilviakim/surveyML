source("R/CCES_0_utilities.R")
load("output/CCES/CCES_perf.RData")
load("output/CCES/CCES_varimp.RData")
pid_vec <- c("pid7", "V212d", "CC307a", "v3007")
educ_vec <- c("educ", "V213")

# Within-year, between-set intersection fxn ====================================
vi_intersect <- function(x, y = 1, from = 1, to = 4, top = 20) {
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
        as_tibble()
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
    )%>%
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
vi_intersect2 <- function(x, y = 1, set = 4, top = 20) {
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
vi_intersect(vid, y = 1, from = 1, to = 4, top = 20) ## Nothing until top 30
tab <- vi_intersect(
  within(vid, rm("year2006")), y = 1, from = 1, to = 2, top = 10
) %>%
  xtable(
    label = "tab:cces_vi_btw_sets_demo_1",
    caption = "Demographics Remaining Within Top 10 Variables, PID Included"
  )
print(
  tab, digits = 0, include.rownames = FALSE, booktabs = TRUE
)

tab <- vi_intersect(
  within(vid, rm("year2006")), y = 1, from = 1, to = 3, top = 10
) %>%
  xtable(
    label = "tab:cces_vi_btw_sets_demo_2",
    caption = 
      "Demographics Remaining Within Top 10 Variables, PID/Issues Included"
  )
print(
  tab, digits = 0, include.rownames = FALSE, booktabs = TRUE
)

## Between years within-set? Again, lots of manual work
vi_intersect2(set = 1, top = 5, vid) ## Black, age, gender
vi_intersect2(set = 2, top = 5, vid) ## Dominated by "Strong Republican"
vi_intersect2(set = 3, top = 5, vid)

# Varimp over time fxn =========================================================
vi_overtime_noneduc <- function(x, y = 1, set = 4) {
  x %>%
    map(y) %>%
    map("rf") %>%
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
    bind_rows(.id = "Year") %>%
    mutate(Year = as.integer(gsub("year", "", Year))) %>%
    pivot_longer(
      -Year, names_to = "Demographics", values_to = "Variable Importance"
    ) %>%
    ggplot(
      aes(
        x = Year, y = `Variable Importance`,
        group = Demographics, colour = Demographics, fill = Demographics,
        linetype = Demographics
      )
    ) + 
    geom_line() +
    scale_color_discrete() +
    scale_linetype_discrete()
}

vi_overtime_educ <- function(x, y = 1, set = 4) {
  x %>%
    map(y) %>%
    map("rf") %>%
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
    bind_rows(.id = "Year") %>%
    mutate(Year = as.integer(gsub("year", "", Year))) %>%
    pivot_longer(
      -Year, names_to = "Demographics", values_to = "Variable Importance"
    ) %>%
    ggplot(
      aes(
        x = Year, y = `Variable Importance`,
        group = Demographics, colour = Demographics, fill = Demographics,
        linetype = Demographics
      )
    ) + 
    geom_line() +
    scale_color_discrete() +
    scale_linetype_discrete()
}

vi_overtime_pid <-  function(x, y = 1, set = 4) {
  x %>%
    map(y) %>%
    map("rf") %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(
            rownames %in% c(
              "pid7.2", "v212d_not_very_strong_democrat", "CC307a.2"
            )
          ) %>%
          select(`Not very strong Democrat` = Overall),
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
          select(`Not very strong Republican` = Overall),
        .x %>%
          filter(
            rownames %in% c("pid7.7", "v212d_strong_democrat", "CC307a.7")
          ) %>%
          select(`Strong Republican` = Overall)
      )
    ) %>%
    bind_rows(.id = "Year") %>%
    mutate(Year = as.integer(gsub("year", "", Year))) %>%
    pivot_longer(
      -Year, names_to = "Demographics", values_to = "Variable Importance"
    ) %>%
    ggplot(
      aes(
        x = Year, y = `Variable Importance`,
        group = Demographics, colour = Demographics, fill = Demographics,
        linetype = Demographics
      )
    ) + 
    geom_line() +
    scale_x_continuous(breaks = seq(2008, 2018, by = 2)) + 
    scale_color_discrete() +
    scale_linetype_discrete()
}

# Importance of race over the years ============================================
## Restricted to set 2, not 4!
p1 <- vi_overtime_noneduc(vid, set = 2)
p2 <- vi_overtime_educ(   vid, set = 2)
p3 <- vi_overtime_noneduc(vid, set = 3)
p4 <- vi_overtime_educ(   vid, set = 3)
p5 <- vi_overtime_noneduc(vid, set = 4)
p6 <- vi_overtime_educ(   vid, set = 4)
p7 <- vi_overtime_pid(   vid, set = 2) + 
  labs(caption = "Base category: Strong Democrat")
p8 <- vi_overtime_pid(   vid, set = 3) + 
  labs(caption = "Base category: Strong Democrat")
p9 <- vi_overtime_pid(   vid, set = 4) + 
  labs(caption = "Base category: Strong Democrat")

## But does this suggest more alignment?  
pdf("fig/CCES/rf/cces_varimp_overtime_set2.pdf", width = 6, height = 4)
Kmisc::pdf_default(p1)
dev.off()
pdf("fig/CCES/rf/cces_varimp_overtime_set3.pdf", width = 6, height = 4)
Kmisc::pdf_default(p2)
dev.off()
pdf("fig/CCES/rf/cces_varimp_overtime_set4.pdf", width = 6, height = 4)
Kmisc::pdf_default(p3)
dev.off()


pdf("fig/CCES/rf/cces_varimp_overtime_set2_pid.pdf", width = 6, height = 4)
Kmisc::pdf_default(p7)
dev.off()
pdf("fig/CCES/rf/cces_varimp_overtime_set3_pid.pdf", width = 6, height = 4)
Kmisc::pdf_default(p8)
dev.off()
pdf("fig/CCES/rf/cces_varimp_overtime_set4_pid.pdf", width = 6, height = 4)
Kmisc::pdf_default(p9)
dev.off()
