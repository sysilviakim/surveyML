source(here::here("R", "utilities.R"))
load(here("data", "anes-tidy", "anes-vl.Rda"))

if (!dir.exists(here("output/ANES/logit"))) {
  dir.create(here("output/ANES/logit"), recursive = TRUE)
  dir.create(here("output/ANES/cart"), recursive = TRUE)
  dir.create(here("output/ANES/rf"), recursive = TRUE)
}

load(here("data", "anes-tidy", "anes_onehot_prez.Rda"))
## essentially anes_sets, just ordered for convenience
for (varset in anes_sets) {
  for (yr in as.character(anes_years)) {
    temp <- anes_onehot[[as.character(yr)]] %>%
      imap(
        ~ {
          if ("data.frame" %in% class(.x)) {
            ## applies to objects such as training and testing dataset
            ## redundant for this iteration of vote choice
            .x %>% select(-pid3, -pid7)
          } else {
            .x
          }
        }
      )
    
    ## vl already loaded
    if (varset < 4) {
      temp$train <- temp$train %>% 
        select(contains(vl[seq(varset)] %>% unlist() %>% paste(sep = "|")))
    } else if (varset > 4) {
      temp$train <- temp$train %>% 
        select(
          ## demo. + extra for Appendix
          contains(vl[c(1, (varset - 1))] %>% unlist() %>% paste(sep = "|"))
        )
    }
    
    message(paste0("Proceeding for year ", yr))
    message(paste0("Variable set is ", varset))
    message(paste0("# of columns in train set: ", dim(temp$train)[2]))
    
    ### Logit
    method <- "logit"
    turn.logit <- train_1line(temp, method = method)
    save(turn.logit, file = file_path_fxn(data = "ANES"))
    rm(turn.logit)
    gc(reset = TRUE)
    message("Logit finished.")
    
    ### CART
    method <- "cart"
    turn.cart <- train_1line(temp, method = "rpart")
    save(turn.cart, file = file_path_fxn(data = "ANES"))
    rm(turn.cart)
    gc(reset = TRUE)
    message("CART finished.")
    
    ### Random Forests
    method <- "rf"
    turn.rf <- train_1line(temp, method = method)
    save(turn.rf, file = file_path_fxn(data = "ANES"))
    rm(turn.rf)
    gc(reset = TRUE)
    message("Random Forests finished.")    
  }
}
