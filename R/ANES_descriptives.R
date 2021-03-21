source(here::here("R", "utilities.R"))
load(here("data", "anes-tidy", "anes-vl.RData"))
load(here("data", "anes-tidy", paste0("anes_prezvote.RData")))

# Take the training set, leave age and missingness out
anes_demo <- anes_onehot %>%
  map(
    ~ .x$train %>%
      select(
        matches(
          setdiff(vl$set1, c("depvar", "VCF0101")) %>% paste(collapse = "|")
        )
      ) %>%
      select(-matches("999"))
  )

# Don't use 1952; they don't have, for example, Hispanic
test <- names(anes_demo$`2016`) %>%
  set_names(., .) %>%
  imap_dfr(
    ~ anes_demo %>%
      imap(
        function(x, y) {
          prop(x, vars = .x, print = FALSE) %>%
            as.data.frame() %>%
            ## second row is `1`
            .[2, ] %>%
            as_tibble(.name_repair = "unique") %>%
            `colnames<-`(y) %>%
            mutate(var = .y) %>%
            select(var, everything())
        }
      ) %>%
      Reduce(left_join, .)
  ) %>%
  mutate(
    var = case_when(
      var == "vcf0104_2" ~ "Female",
      var == "vcf0104_3" ~ "Gender: other",
      var == "vcf0105b_2" ~ "Black",
      var == "vcf0105b_3" ~ "Hispanic",
      var == "vcf0105b_4" ~ "Race: other/multiple",
      var == "vcf0110_2" ~ "Education: high school",
      var == "vcf0110_3" ~ "Education: some college",
      var == "vcf0110_4" ~ "Education: college+",
      var == "vcf0114_2" ~ "Income: 17-33 percentile",
      var == "vcf0114_3" ~ "Income: 34-67 percentile",
      var == "vcf0114_4" ~ "Income: 68-95 percentile",
      var == "vcf0114_5" ~ "Income: 96-100 percentile"
    )
  ) %>%
  rename(Variable = var)

print(
  xtable(
    test,
    caption = "Distribution of Demographics in Percentages, ANES 1952--2016",
    label = "tab:anes_demo_dist"
  ),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "anes_demo_dist.tex")
)
