source(here::here("R", "utilities.R"))
source(here("R", "NS_extract_performance.R"))
load(here("output/CCES/CCES_perf.RData"))
load(here("output/CCES/CCES_varimp.RData"))

# CCES performance into a table (pres vote choice, 2008-2018) ==================
summ_df <- seq(4) %>%
  map(
    ~ perf_summ(
      within(perf, rm("year2006")), 1, "rf", .x, yr = rev(seq(2008, 2018, 2))
    )
  ) %>%
  bind_rows(.id = "Set")

# Append NS outputs (pres vote choice, 2020) per Jan's code source outputs =====
set.seed(123)
NS_auc_range <- seq(4) %>%
  map(
    ~ predict(
      eval(parse(text = paste0("NS", .x)))$finalModel, 
      data = Xm_testSet_adjusted
    ) %>%
      .[["predictions"]] %>%
      .[, "Biden"]
  ) %>%
  map(
    ~ ci.auc(
      as.numeric(Xm_testSet_adjusted$trump2Pvote_intent) - 1, .x,
      method = "bootstrap"
    )
  )

summ_df <- summ_df %>%
  bind_rows(
    .,
    data.frame(
      Set = as.character(seq(4)),
      Year = 2020,
      AUC = AUC_Nationscape,
      AUC_lower = NS_auc_range %>% map(1) %>% unlist(),
      AUC_upper = NS_auc_range %>% map(3) %>% unlist(),
      Accuracy = Accuracy_Nationscape,
      Accuracy_lower = AN$Lower,
      Accuracy_upper = AN$Upper,
      CI = map2(
        AN$Lower, AN$Upper,
        ~ paste0(
          "[",  str_pad(round(.x, digits = 4), 6, "right", "0"),
          ", ", str_pad(round(.y, digits = 4), 6, "right", "0"), "]"
        )
      ) %>%
        unlist() %>%
        unname(),
      Precision = c(
        S1$byClass[c("Precision")], S2$byClass[c("Precision")],
        S3$byClass[c("Precision")], S4$byClass[c("Precision")]
      ),
      Recall = c(
        S1$byClass[c("Recall")], S2$byClass[c("Recall")],
        S3$byClass[c("Recall")], S4$byClass[c("Recall")]
      ),
      F1 = F1_Nationscape,
      stringsAsFactors = FALSE
    )
  ) %>%
  mutate(Set = factor(Set, levels = seq(4), labels = set_labels)) %>%
  arrange(desc(Year), Set)

save(summ_df, file = here("data/cces-tidy/perf_summ_CCES_Nationscape.Rda"))
