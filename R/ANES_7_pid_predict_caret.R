source(here::here("R", "utilities.R"))
load(here("data", "anes-tidy", "anes-vl.RData"))

sfx <- "prezvote"
load(here("data", "anes-tidy", paste0("anes_", sfx, ".RData")))

tc <- trainControl(
  method = "cv",
  number = 10,
  ## Provides ROC summary stats
  summaryFunction = caret::multiClassSummary,
  allowParallel = TRUE,
  verboseIter = FALSE,
  seeds = rep_seeds(),
  classProbs = TRUE,
  savePredictions = "final"
)

for (yr in as.character(anes_years)) {
  ## so that files are not overwritten
  for (varset in seq(9, 10)) {
    if (varset == 9) {
      temp <- anes_onehot[[as.character(yr)]] %>%
        imap(
          ~ if (any(class(.x) == "data.frame")) {
            .x %>%
              select(-depvar) %>%
              select(depvar = pid7, everything()) %>%
              filter(!is.na(depvar) & depvar %in% seq(7)) %>%
              mutate(
                depvar = factor(depvar, levels = seq(7), labels = pid_labels)
              ) %>%
              select(contains(vl["set1"] %>% unlist() %>% paste(sep = "|")))
          } else {
            .x
          }
        )
    } else {
      temp <- anes_onehot[[as.character(yr)]] %>%
        imap(
          ~ if (any(class(.x) == "data.frame")) {
            .x %>%
              select(-depvar) %>%
              select(depvar = pid3, everything()) %>%
              filter(!is.na(depvar) & depvar %in% seq(3)) %>%
              mutate(
                depvar = factor(
                  depvar,
                  levels = c(1, 2, 3),
                  labels = c("democrat", "indepdent", "republican")
                )
              ) %>%
              select(contains(vl["set1"] %>% unlist() %>% paste(sep = "|")))
          } else {
            .x
          }
        )
    }

    message(paste0("Proceeding for year ", yr))
    message(paste0("Variable set is ", varset))
    message(paste0("# of columns in train set: ", dim(temp$train)[2]))

    ### Logit
    method <- "logit"
    turn.logit <- train_1line(temp, method = method, tc = tc, metric = "prAUC")
    save(turn.logit, file = file_path_fxn(data = "ANES"))
    rm(turn.logit)
    gc(reset = TRUE)
    message("Logit finished.")

    ### CART
    method <- "cart"
    turn.cart <- train_1line(temp, method = "rpart", tc = tc, metric = "prAUC")
    save(turn.cart, file = file_path_fxn(data = "ANES"))
    rm(turn.cart)
    gc(reset = TRUE)
    message("CART finished.")

    ### Random Forests
    method <- "rf"
    turn.rf <- train_1line(temp, method = method, tc = tc, metric = "prAUC")
    save(turn.rf, file = file_path_fxn(data = "ANES"))
    rm(turn.rf)
    gc(reset = TRUE)
    message("Random Forests finished.")
  }
}
