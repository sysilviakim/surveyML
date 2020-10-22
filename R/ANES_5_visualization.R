source("R/CCES_0_utilities.R")
load("output/ANES/ANES_perf.RData")
load("output/ANES/ANES_varimp.RData")

# Varimp over time fxn =========================================================
vi_overtime_noneduc <- function(x, y = 1, set = 4) {
  x %>%
    map(y) %>%
    map("rf") %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>% 
          filter(grepl("VCF0104", rownames)) %>% select(Gender = Overall),
        .x %>% 
          filter(grepl("VCF0101", rownames)) %>% select(Age = Overall),
        .x %>%
          filter(grepl("VCF0105b.2", rownames)) %>% select(Black = Overall),
        .x %>%
          filter(grepl("VCF0105b.3", rownames)) %>% select(Hispanic = Overall),
        .x %>%
          filter(grepl("VCF0105b.3", rownames)) %>% select(OtherRace = Overall),
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
