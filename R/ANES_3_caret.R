source("R/utilities.R")
load(file.path("data", "anes-tidy", "anes_prezvote_onehot.RData"))
load(file.path("data", "anes-tidy", "anes-vl.RData"))

sfx <- "prezvote"
for (yr in as.character(seq(1952, 2016, by = 4))) {
  for (varset in seq(4)) {
    temp <- anes_onehot[[as.character(yr)]]
    
    ## vl already loaded
    if (varset < 4) {
      temp$train <- temp$train %>% 
        select(
          contains(vl[seq(varset)] %>% unlist() %>% paste(sep = "|"))
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
