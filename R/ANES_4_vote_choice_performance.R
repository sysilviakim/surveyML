source(here::here("R", "utilities.R"))
load(here("data", "variable_labels.Rda"))

if (!dir.exists(here("tab", "perf"))) {
  dir.create(here("tab", "perf"))
}

## Extract performance measures ================================================
if (!file.exists(here("output/ANES/ANES_perf.Rda"))) {
  vid <- perf <- list()
  
  load(here("data", "anes-tidy", "anes_onehot_prez.Rda"))
  for (yr in as.character(anes_years)) {
    temp <- anes_onehot[[as.character(yr)]]
    
    ## caret results
    for (method in c("logit", "cart", "rf")) {
      for (varset in anes_sets) {
        ## Load previously run results
        load(
          here(
            "output", "ANES", method,
            paste0(method, "_", yr, "_st", varset, ".Rda")
          )
        )
        
        ## Evaluate performance
        perf[[paste0("year", yr)]][[method]][[paste0("set", varset)]] <-
          perf_routine(
            method,
            eval(parse(text = paste0("turn.", method))),
            temp$test,
            dv = levels(temp$test$depvar)
          )
        
        ## Plot variable importance
        pdf_varimp(
          eval(parse(text = paste0("turn.", method))),
          here(
            "fig", "ANES", method,
            paste0("var", method, "_", yr, "_st", varset, ".pdf")
          ),
          labels = vl[["anes"]],
          font = "CM Roman"
        )
        
        vid_temp <- varImp(eval(parse(text = paste0("turn.", method))))
        vid[[paste0("year", yr)]][[method]][[paste0("set", varset)]] <-
          vid_temp$importance %>% Kmisc::rowid_matrix_to_df()
        
        ## Save memory
        rm(list = paste0("turn.", method))
        gc(reset = TRUE)
        save(perf, file = here("output/ANES/ANES_perf.Rda"))
        save(vid, file = here("output/ANES/ANES_varimp.Rda"))
      }
    }
  }
}

# xtable export ================================================================
load(here("output/ANES/ANES_perf.Rda"))
load(here("output/ANES/ANES_varimp.Rda"))

summ_df <- c("logit", "cart", "rf") %>%
  set_names(., .) %>%
  map(
    ~ anes_sets %>%
      set_names(., .) %>%
      map(
        function(x) perf_summ(perf, .x, x, yr = anes_years)
      ) %>%
      bind_rows(.id = "Set") %>%
      arrange(desc(Year), Set) %>%
      mutate(
        Set = factor(
          Set,
          levels = anes_sets, labels = set_labels
        )
      ) %>%
      rename(`Variable Specification` = Set) %>%
      select(-AUC_lower, -AUC_upper, -Accuracy_lower, -Accuracy_upper)
  )

summ_df %>%
  imap(
    ~ print(
      .x %>%
        xtable(
          label = paste0("tab:ANES_prezvote_", .y),
          caption = paste0(
            "Performance Metrics, Presidential Vote Choice, ",
            ifelse(
              .y == "logit", "Logit, ",
              ifelse(.y == "cart", "CART, ", "Random Forests, ")
            ),
            "ANES 1952--2020"
          ),
          digits = c(0, 0, 0, 4, 4, 4, 4, 4, 4)
        ),
      include.rownames = FALSE,
      file = here(
        "tab", "perf", paste0("ANES_", .y, ".tex")
      ),
      booktabs = TRUE, floating = FALSE, tabular.environment = "longtable"
    )
  )
save(summ_df, file = here("output", "summ_list.Rda"))

# Alternative, just accuracy between methods ===================================
tab <- c("logit", "cart", "rf") %>%
  map(
    ~ anes_sets %>%
      set_names(., .) %>%
      map_dfr(
        function(x) {
          perf_summ(perf, .x, x, yr = anes_years) %>%
            select(Year, !!as.name(.x) := CI) %>%
            mutate(Set = x)
        }
      ) %>%
      select(Set, everything()) %>%
      arrange(desc(Year), Set) %>%
      mutate(Set = factor(Set, levels = anes_sets, labels = set_labels)) %>%
      rename(`Variable Specification` = Set)
  ) %>%
  Reduce(left_join, .) %>%
  rename(Logit = logit, CART = cart, RF = rf) %>%
  xtable(
    label = paste0("tab:ANES_prezvote_accuracy"),
    caption = paste0(
      "Accuracy Range Comparison, ", "Presidential Vote Choice, ",
      "ANES 1952--2020"
    ),
    digits = 0
  )
print(
  tab,
  include.rownames = FALSE,
  file = here("tab", "perf", paste0("ANES_prezvote_accuracy.tex")),
  booktabs = TRUE, floating = FALSE, tabular.environment = "longtable"
)

# Quick check ==================================================================
perf_df <- summ_df$rf
set_names(set_labels, set_labels) %>%
  map(
    ~ perf_df %>% 
      filter(`Variable Specification` == .x) %>%
      lm(Accuracy ~ Year, data = .) %>%
      summary()
  )

set_names(set_labels, set_labels) %>%
  map_dbl(
    ~ perf_df %>% 
      filter(`Variable Specification` == .x) %>%
      .$Accuracy %>%
      mean()
  )

p_list <- unique(perf$`Variable Specification`) %>%
  set_names(., .) %>%
  map(
    ~ perf %>% 
      filter(`Variable Specification` == .x) %>%
      ggplot(.) + 
      geom_line(aes(x = Year, y = Accuracy))
  )

vid %>% map(~ .x$rf$set7 %>% slice_max(Overall, n = 5))
