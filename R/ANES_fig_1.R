source(here::here("R", "utilities.R"))

# RF (Fig 1A): data import =====================================================
## Import summarized performance (vote choice, president, rf)
perf <- list(
  CCNS = loadRData(here("output/CCES/perf_summ_CCES_Nationscape.Rda")) %>%
    mutate(Survey = "CCES/NS"),
  ANES = loadRData(here("output/ANES/perf_summ_ANES_prezvote_rf.Rda")) %>%
    mutate(Survey = "ANES")
) %>%
  bind_rows() %>%
  mutate(Set = fct_relevel(Set, set_labels[c(1, 2, 7, 3, 4, 5, 6, 8)])) %>%
  filter(Set == "Demographics Only") %>%
  mutate(Set = "Vote Choice") %>%
  filter(Survey == "ANES") %>%
  bind_rows(
    ## # Import summarized performance (PID, 7-pt, rf)
    ., loadRData(
      here("output/ANES/", paste0("perf_summ_ANES_prezvote_rf_pid.Rda"))
    ) %>%
      rename(Set = Y) %>%
      filter(Set == "3-pt PID") %>%
      mutate(Survey = "ANES")
  )

## Draw
p <- po_full(
  perf, "Accuracy",
  ylim = c(0, 1), name = "Y-var",
  colour_nrow = 1, linetype_nrow = 1, end = 0.85, accrange = TRUE, y2 = TRUE
)
p <- pdf_default(p) +
  theme(legend.position = "bottom", legend.key.width = unit(.9, "cm"))
p

pdf(here("fig", "survey_rf_2ys_accrange.pdf"), width = 6.2, height = 4)
print(p)
dev.off()

# Logit (Fig 1B): data import ==================================================
## Import summarized performance (vote choice, president, rf)
perf <- list(
  CCNS = loadRData(here("output/CCES/perf_summ_CCES_Nationscape_Logit.Rda")) %>%
    mutate(Survey = "CCES/NS"),
  ANES = loadRData(here("output/ANES/perf_summ_ANES_prezvote_logit.Rda")) %>%
    mutate(Survey = "ANES")
) %>%
  bind_rows() %>%
  mutate(Set = fct_relevel(Set, set_labels[c(1, 2, 7, 3, 4, 5, 6, 8)])) %>%
  filter(Set == "Demographics Only") %>%
  mutate(Set = "Vote Choice") %>%
  filter(Survey == "ANES") %>%
  bind_rows(
    ## # Import summarized performance (PID, 7-pt, rf)
    ., loadRData(
      here("output/ANES/", paste0("perf_summ_ANES_prezvote_logit_pid.Rda"))
    ) %>%
      rename(Set = Y) %>%
      filter(Set == "3-pt PID") %>%
      mutate(Survey = "ANES")
  )

## Draw
p <- po_full(
  perf, "Accuracy",
  ylim = c(0, 1), name = "Y-var",
  colour_nrow = 1, linetype_nrow = 1, end = 0.85, accrange = TRUE, y2 = TRUE
)
## geom_pointrange(aes(ymin = Accuracy_lower, ymax = Accuracy_upper))
p <- pdf_default(p) +
  theme(legend.position = "bottom", legend.key.width = unit(.9, "cm"))
p

pdf(here("fig", "survey_logit_2ys_accrange.pdf"), width = 6.2, height = 4)
print(p)
dev.off()
