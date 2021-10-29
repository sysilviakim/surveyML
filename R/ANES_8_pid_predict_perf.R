source(here::here("R", "utilities.R"))
load(here("data", "variable_labels.Rda"))
vid <- perf <- list()

sfx <- "pid"
load(here("data", "anes-tidy", paste0("anes_", sfx, ".RData")))

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

for (yr in names(anes_onehot_pid)) {
  ## caret results
  for (method in c("logit", "cart", "rf", "ol")) {
    for (varset in seq(9, 11)) {
      if (varset == 9) {
        temp <- anes_onehot_pid[[as.character(yr)]] %>%
          imap(
            ~ if (any(class(.x) == "data.frame")) {
              .x %>%
                select(-depvar) %>%
                select(depvar = pid7, everything()) %>%
                filter(!is.na(depvar) & depvar %in% seq(7)) %>%
                mutate(
                  depvar = factor(depvar, levels = seq(7), labels = pid_labels)
                )
            } else {
              .x
            }
          )
      } else if (varset == 10) {
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
      
      if (!(method == "ol" & varset == 11)) {
        ## Load previously run results
        load(
          here(
            "output", "ANES", method,
            paste0(method, "_", yr, "_", sfx, "_st", varset, ".RData")
          )
        )
        
        ## Evaluate performance
        perf[[paste0("year", yr)]][[sfx]][[method]][[paste0("set", varset)]] <-
          pred_df(temp, eval(parse(text = paste0("turn.", method))))
        
        ## Plot variable importance
        pdf_varimp(
          eval(parse(text = paste0("turn.", method))),
          here(
            "fig", "ANES", method,
            paste0("var", method, "_", yr, "_", sfx, "_st", varset, ".pdf")
          ),
          labels = vl[["anes"]],
          font = "CM Roman"
        )
        
        vid_temp <- varImp(eval(parse(text = paste0("turn.", method))))
        vid[[paste0("year", yr)]][[sfx]][[method]][[paste0("set", varset)]] <-
          vid_temp$importance %>% Kmisc::rowid_matrix_to_df()
        
        ## Save memory
        rm(list = paste0("turn.", method))
        gc(reset = TRUE)
        save(perf, file = here("output/ANES/ANES_perf_pid.RData"))
        save(vid, file = here("output/ANES/ANES_varimp_pid.RData"))
      }
    }
  }
}
