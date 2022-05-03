source(here::here("R", "utilities.R"))
load(here("data", "anes-tidy", "anes-vl.Rda"))
vl$set7 <- c(vl$set1, vl$set7)
load(here("data", "anes-tidy", "anes_onehot_pid.Rda"))

# if (!dir.exists(here("output/ANES/ol"))) {
#   dir.create(here("output/ANES/ol"), recursive = TRUE)
#   dir.create(here("fig/ANES/ol"), recursive = TRUE)
# }

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

## so that files are not overwritten
for (varset in seq(9, 14)) {
  for (yr in setdiff(names(anes_onehot_pid), "1948")) {
    if (varset %in% seq(9, 10)) {
      ## 7-point party ID
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
                  levels = seq(7), labels = pid_labels, ordered = TRUE
                )
              )
          } else {
            .x
          }
        )
    } else if (varset %in% seq(11, 12)) {
      ## 3-point party ID
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
                  levels = c(1, 2, 3),
                  labels = c("democrat", "independent", "republican")
                )
              )
          } else {
            .x
          }
        )
    } else {
      ## binary party ID
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

    ## set1 and set7 (comprehensive demo)
    if (varset %% 2 == 0) {
      temp <- temp %>%
        imap(
          ~ if (any(class(.x) == "data.frame")) {
            .x %>%
              select(contains(vl["set7"] %>% unlist() %>% paste(sep = "|")))
          } else {
            .x
          }
        )
    } else {
      temp <- temp %>%
        imap(
          ~ if (any(class(.x) == "data.frame")) {
            .x %>%
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
    save(turn.logit, file = file_path_fxn(data = "ANES", pid = TRUE))
    rm(turn.logit)
    gc(reset = TRUE)
    message("Logit finished.")

    ### CART
    method <- "cart"
    turn.cart <- train_1line(temp, method = "rpart", tc = tc, metric = "prAUC")
    save(turn.cart, file = file_path_fxn(data = "ANES", pid = TRUE))
    rm(turn.cart)
    gc(reset = TRUE)
    message("CART finished.")

    ### Random Forests
    method <- "rf"
    turn.rf <- train_1line(temp, method = method, tc = tc, metric = "prAUC")
    save(turn.rf, file = file_path_fxn(data = "ANES", pid = TRUE))
    rm(turn.rf)
    gc(reset = TRUE)
    message("Random Forests finished.")

    ## Ordered choice, MASS polr (logistic)
    # if (varset < 13) {
    #   method <- "ol"
    #   turn.ol <- train_1line(temp, method = method, tc = tc, metric = "prAUC")
    #   save(turn.ol, file = file_path_fxn(data = "ANES", pid = TRUE))
    #   rm(turn.ol)
    #   gc(reset = TRUE)
    #   message("Random Forests finished.")
    # }
    ## initial value in 'vmmin' is not finiteError in
    ## starting varset 12 yr 1968
  }
}
