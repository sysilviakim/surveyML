source(here::here("R", "utilities.R"))
load(here("data", "variable_labels.Rda"))
load(here("data", "anes-tidy", "anes_onehot_pid.Rda"))

# Functions ====================================================================
## Because this is multiclass, not binary
pred_df <- function(list, model, y = "depvar") {
  list$test$pred <- predict.train(model, newdata = list$test)
  table(list$test$pred)
  temp <- list$test %>%
    select(obs = !!as.name(y), pred = pred) %>%
    bind_cols(., predict.train(model, newdata = list$test, type = "prob"))
  temp <- as.data.frame(temp) ## If tibble, levels(temp[, "pred"]) does not work
  return(
    list(
      cf.matrix =
        confusionMatrix(data = list$test$pred, reference = list$test[[y]]),
      pred = temp,
      multiclass = multiClassSummary(temp, lev = levels(list$test$depvar))
    )
  )
}

perf_summ <- function(perf, method, set, yr) {
  data.frame(
    Year = yr,
    Accuracy = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map_dbl(~ .x$cf.matrix$overall[["Accuracy"]]),
    Accuracy_lower = perf %>% map(method) %>%
      map(paste0("set", set)) %>%
      map_dbl(~ .x$cf.matrix$overall[["AccuracyLower"]]),
    Accuracy_upper = perf %>% map(method) %>%
      map(paste0("set", set)) %>%
      map_dbl(~ .x$cf.matrix$overall[["AccuracyUpper"]]),
    CI = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$cf.matrix$overall) %>%
      map(
        ~ paste0(
          "[",
          str_pad(round(.x[["AccuracyLower"]], digits = 4), 6, "right", "0"),
          ", ",
          str_pad(round(.x[["AccuracyUpper"]], digits = 4), 6, "right", "0"),
          "]"
        )
      ) %>%
      unlist(),
    AUC = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map_dbl(~ .x$multiclass[["AUC"]]),
    # prAUC = perf %>% map(method) %>% map(paste0("set", set)) %>%
    #   map_dbl(~ .x$multiclass[["prAUC"]]),
    Precision = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map_dbl(
        ~ .x$multiclass[[
        names(.x$multiclass)[grepl("Precision", names(.x$multiclass))]]]
      ),
    Recall = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map_dbl(
        ~ .x$multiclass[[
          names(.x$multiclass)[grepl("Recall", names(.x$multiclass))]]]
      ),
    row.names = NULL
  ) %>%
    mutate(F1 = 2 * (Precision * Recall) / (Precision + Recall))
}

# Loop =========================================================================
if (!file.exists(here("output/ANES/ANES_perf_pid.Rda"))) {
  vid <- perf <- list()

  for (yr in setdiff(names(anes_onehot_pid), "1948")) {
    ## caret results
    for (method in c("logit", "cart", "rf")) {
      for (varset in anes_sets_pid) {
        if (varset %in% seq(10, 11)) {
          temp <- anes_onehot_pid[[as.character(yr)]] %>%
            imap(
              ~ if (any(class(.x) == "data.frame")) {
                .x %>%
                  select(-depvar) %>%
                  select(depvar = pid7, everything()) %>%
                  filter(!is.na(depvar) & depvar %in% seq(7)) %>%
                  mutate(
                    depvar = factor(
                      depvar,
                      levels = seq(7), labels = pid_labels
                    )
                  )
              } else {
                .x
              }
            )
        } else if (varset %in% seq(12, 13)) {
          temp <- anes_onehot_pid[[as.character(yr)]] %>%
            imap(
              ~ if (any(class(.x) == "data.frame")) {
                .x %>%
                  select(-depvar) %>%
                  select(depvar = pid3, everything()) %>%
                  filter(!is.na(depvar) & depvar %in% seq(3)) %>%
                  mutate(
                    depvar = factor(
                      depvar,
                      levels = seq(3),
                      labels = c("democrat", "independent", "republican")
                    )
                  )
              } else {
                .x
              }
            )
        } else {
          temp <- anes_onehot_pid[[as.character(yr)]] %>%
            imap(
              ~ if (any(class(.x) == "data.frame")) {
                .x %>%
                  select(-depvar) %>%
                  mutate(depvar = case_when(pid3 == 1 ~ 1, pid3 == 3 ~ 0)) %>%
                  select(depvar, everything()) %>%
                  filter(!is.na(depvar)) %>%
                  mutate(
                    depvar = factor(
                      depvar,
                      levels = c(0, 1),
                      labels = c("republican", "democrat")
                    )
                  )
              } else {
                .x
              }
            )
        }

        ## Load previously run results
        load(
          here(
            "output", "ANES", method,
            paste0(method, "_", yr, "_st", varset, "_pid.Rda")
          )
        )

        ## Evaluate performance
        perf[[paste0("year", yr)]][[method]][[paste0("set", varset)]] <-
          pred_df(temp, eval(parse(text = paste0("turn.", method))))

        ## Plot variable importance
        pdf_varimp(
          eval(parse(text = paste0("turn.", method))),
          here(
            "fig", "ANES", method,
            paste0("var", method, "_", yr, "_st", varset, "_pid.pdf")
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
        save(perf, file = here("output/ANES/ANES_perf_pid.Rda"))
        save(vid, file = here("output/ANES/ANES_varimp_pid.Rda"))
      }
    }
  }
}

# xtable export ================================================================
load(here("output/ANES/ANES_perf_pid.Rda"))
load(here("output/ANES/ANES_varimp_pid.Rda"))

summ_df_pid <- methods %>%
  map(
    ~ anes_sets_pid %>%
      set_names(., .) %>%
      map(
        function(x) {
          perf_summ(
            perf, .x, x,
            yr = setdiff(names(anes_onehot_pid), "1948")
          )
        }
      ) %>%
      bind_rows(.id = "Set") %>%
      mutate(Year = as.numeric(Year)) %>%
      arrange(desc(Year), Set) %>%
      mutate(
        Set = factor(Set, levels = anes_sets_pid, labels = set_labels_pid)
      ) %>%
      rename(`Variable Specification` = Set)
  )
save(summ_df_pid, file = here("output", "summ_list_pid.Rda"))

# Quick check ==================================================================
perf_df <- summ_df_pid$rf
perf_df %>%
  filter(`Variable Specification` == "Binary PID, Demographics Only") %>%
  .$Accuracy %>%
  mean()
## [1] 0.6342

set_names(set_labels_pid, set_labels_pid) %>%
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

p_list <- unique(perf_df$`Variable Specification`) %>%
  set_names(., .) %>%
  map(
    ~ perf_df %>%
      filter(`Variable Specification` == .x) %>%
      ggplot() +
      geom_line(aes(x = Year, y = Accuracy))
  )
