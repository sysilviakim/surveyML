########################################################################
# Purpose: Extract OOS accuracy and other performance metrics 
########################################################################
Xm$Liberal <- ifelse(Xm$ideo5_1==1 | Xm$ideo5_2==1, 1, 0)
Xm$Moderate <- ifelse(Xm$ideo5_3, 1, 0)
Xm$Conservative <- ifelse(Xm$ideo5_4==1 | Xm$ideo5_5==1, 1, 0)


Xm_testSet_full <- Xm[-D_trainsetIndex_20percent, ]
Xm_testSet_full_GEO <- Xm_GEO[-D_trainsetIndex_20percent, ]

# We want a suitably-size test set
# [we verified that this always produces the same test set,
# as long as NS_2_prep_ML.R and set.seed() therein are executed]
Xm_testSet_adjusted <- Xm_testSet_full %>%
  slice_sample(n = length(D_trainsetIndex_20percent) * .2)

Xm_testSet_adjusted_GEO <- Xm_testSet_full_GEO %>%
  slice_sample(n = length(D_trainsetIndex_20percent) * .2)




# Import model objects
NS1B <- readRDS(here::here("output","Nationscape","R_spec1B.RDS"))
NS1C <- readRDS(here::here("output","Nationscape","R_spec1C.RDS"))
NS1D <- readRDS(here::here("output","Nationscape","R_spec1D.RDS"))
NS1E <- readRDS(here::here("output","Nationscape","R_spec1E.RDS"))

#############################
# GET PERF FOR ALL ALGORITHMS
#############################

get_performance_stats <- function(x, test, dv, depvar, verbose = TRUE) {
  output <- list()
  
  output[["predicted_factor"]] <- predict.train(x, test, type = "raw") # this gives us the predicted class
  output[["pred.probs"]] <- predict.train(x, test, type = "prob")
  p2 <- output[["pred.probs"]][, 2]
  
  # output[["pred.factor"]] <- pred.factor <- factor(
  #   ifelse(p2 < 0.5, dv[1], dv[2]),
  #   levels = dv)
  
  ## Confusion matrix contains accuracy (+CI)
  output[["cf.matrix"]] <- confusionMatrix(output[["predicted_factor"]], test %>% pull({{depvar}}))
  
  
  output[["pred.compare"]] <- pred.compare <- ROCR::prediction(
    p2, as.numeric(test %>% pull({{depvar}})) - 1
  )
  output[["auc"]] <- ROCR::performance(pred.compare, "auc")@y.values[[1]]
  
  output[["Perf"]] <- 
    ROCR::performance(output[["pred.compare"]], "tpr", "fpr")
  output[["Prec"]] <- 
    ROCR::performance(output[["pred.compare"]], "prec", "rec")
  output[["F1"]] <- 
    ROCR::performance(output[["pred.compare"]], "f")
  
  output
}

###############################################################
# Helper function producing a row of performance metrics
###############################################################
get_trump2p <- function(x) {  
  AUC <- get_performance_stats(x,
                               test = Xm_testSet_adjusted,
                               depvar = "trump2Pvote_intent")$auc
  names(AUC) <- "AUC"
  
  ACC <- get_performance_stats(x,
                               test = Xm_testSet_adjusted,
                               depvar = "trump2Pvote_intent")$cf.matrix$overall[c("Accuracy","AccuracyLower","AccuracyUpper")]
  
  STAT3 <- get_performance_stats(x,
                                 test = Xm_testSet_adjusted,
                                 depvar = "trump2Pvote_intent")$cf.matrix$byClass[c("Precision","Recall","F1")]
  
  tmp <- c(AUC,ACC,STAT3)
  
  as_tibble_row(tmp)
}



NS_PERF_TIBBLE <- bind_rows(
  get_trump2p(NS1B),
  
  c(AUC = get_performance_stats(x = NS1C,
                                test = Xm_testSet_adjusted_GEO,
                                depvar = "trump2Pvote_intent")$auc,
    get_performance_stats(x = NS1C,
                          test = Xm_testSet_adjusted_GEO,
                          depvar = "trump2Pvote_intent")$cf.matrix$overall[c("Accuracy","AccuracyLower","AccuracyUpper")],
    get_performance_stats(x = NS1C,
                          test = Xm_testSet_adjusted_GEO,
                          depvar = "trump2Pvote_intent")$cf.matrix$byClass[c("Precision","Recall","F1")]),
  
  get_trump2p(NS1D)
  #get_trump2p(NS3E)
)

NS_PERF_export <- cbind(tibble(`Model and Specification` = 
                                 c("Demo. + Religion",
                                   "Demo. + South",
                                   "Demo. + Symbolic ideology"
                                 )),
                        NS_PERF_TIBBLE 
)



xtable(NS_PERF_export, digits = 3) %>%
  print(table.placement = "H", 
        include.rownames = FALSE, 
        booktabs = TRUE,
        file = here::here("tab","NS_perf_additional_specifications.tex"))

knitr::kable(NS_PERF_export, digits = 2)
