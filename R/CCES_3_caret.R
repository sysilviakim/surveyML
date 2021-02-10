source(here::here("R", "utilities.R"))

if (!dir.exists(here("output/CCES/logit"))) {
  dir.create(here("output/CCES/logit"), recursive = TRUE)
  dir.create(here("output/CCES/cart"), recursive = TRUE)
  dir.create(here("output/CCES/rf"), recursive = TRUE)
}

## loop to enable gc()
## cforest not included

for (yr in rev(seq(2006, 2018, 2))) {
  for (i in seq(length(file_suffix[[paste0("year", yr)]]))) {
    sfx <- file_suffix[[paste0("year", yr)]][i]
    
    ## Default 2018
    vl <- list(
      ## Prevent multrace being mixed in
      set1 = c("depvar", "gender", "race", "educ", "faminc", "birthyr"),
      ## vs. pid7 or CC18_421a or ... equivalent of CC16_421a which disappeared
      set2 = c("pid7"), 
      set3 = c(paste0("CC18_32", seq(0, 8)), "CC18_331", "CC18_332")
    )
    
    for (varset in seq(4)) {
      if (yr == 2016) {
        ## CC16_421a: Generally speaking, do you think of yourself as a ... ?
        ## CC16_360:  With which party, if any, are you registered?
        vl$set3 <- paste0("CC16_33", seq(0, 7))
      } else if (yr == 2014) {
        ## CC350:  With which party, if any, are you registered?
        vl$set3 <- paste0("CC14_33", seq(0, 9))
      } else if (yr == 2012) {
        vl$set3 <- c(paste0("CC32", seq(0, 9)), "CC332")
      } else if (yr == 2010) {
        vl$set1 <- c("depvar", "V208", "V211", "V213", "V246", "V207")
        vl$set2 <- c("V212d")
        vl$set3 <- c(paste0("CC32", seq(0, 9)), "CC332")
      } else if (yr == 2008) {
        vl$set1 <- c("depvar", "V208", "V211", "V213", "V246", "V207")
        vl$set2 <- c("CC307a")
        ## (This question does not exist in next surveys)
        ## What is the most important problem facing the country today?
        vl$set3 <- c("CC301", "CC309", paste0("CC31", seq(0, 6)))
      } else if (yr == 2006) {
	      vl$set1 <- c("depvar", "v2004", "v2005", "v2018", "v2032", "v2020")
	      vl$set2 <- c("v3007")
	      vl$set3 <- c("v3010", "v3019", paste0("v", seq(3021, 3035)))
	    }
      ## set4 is full set
      load(here("data", "cces-tidy", paste0("data_", yr, "_", sfx, ".RData")))
      if (varset < 4) {
        temp$train <- temp$train %>% 
          select(
            contains(vl[seq(varset)] %>% unlist() %>% paste(sep = "|"))
          ) %>%
          select(-contains("multrace"))
      }
      
      message(paste0("Proceeding for year ", yr))
      message(paste0("Y variable is ", sfx))
      message(paste0("Variable set is ", varset))
      message(paste0("# of columns in train set: ", dim(temp$train)[2]))
      
      ### Logit
      method <- "logit"
      turn.logit <- train_1line(temp, method = method)
      save(turn.logit, file = file_path_fxn())
      rm(turn.logit)
      gc(reset = TRUE)
      message("Logit finished.")

      ### CART
      method <- "cart"
      turn.cart <- train_1line(temp, method = "rpart")
      save(turn.cart, file = file_path_fxn())
      rm(turn.cart)
      gc(reset = TRUE)
      message("CART finished.")
      
      ### Random Forests
      method <- "rf"
      turn.rf <- train_1line(temp, method = method)
      save(turn.rf, file = file_path_fxn())
      rm(turn.rf)
      gc(reset = TRUE)
      message("Random Forests finished.")
    }
    
    message(paste0("Year ", yr, " done."))
  }
}
