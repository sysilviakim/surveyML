source("R/NS_0_labels.R")
source("R/NS_1D_data_prep.R")
source("R/NS_2_prep_ML.R")
########################################################################
# Purpose: Extract OOS accuracy and other performance metrics 
########################################################################

Xm_testSet_full <- Xm[-D_trainsetIndex_20percent, ]

# We want a suitably-size test set
# [we verified that this always produces the same test set,
# as long as NS_2_prep_ML.R and set.seed() therein are executed]
Xm_testSet_adjusted <- Xm_testSet_full %>%
  slice_sample(n = length(D_trainsetIndex_20percent) * .2)


# Import model objects
NS1 <- readRDS(here::here("output","Nationscape","ranger_Nationscape_D1.RDS"))
NS2 <- readRDS(here::here("output","Nationscape","ranger_Nationscape_D2.RDS"))
NS3 <- readRDS(here::here("output","Nationscape","ranger_Nationscape_D3.RDS"))
NS4 <- readRDS(here::here("output","Nationscape","ranger_Nationscape_D4.RDS"))

NS_logit__spec1 <- readRDS(here::here("output","Nationscape","logit_Nationscape_D1.RDS"))
NS_logit__spec2 <- readRDS(here::here("output","Nationscape","logit_Nationscape_D2.RDS"))
NS_logit__spec3 <- readRDS(here::here("output","Nationscape","logit_Nationscape_D3.RDS"))
NS_logit__spec4 <- readRDS(here::here("output","Nationscape","logit_Nationscape_D4.RDS"))

NS_CART_spec1 <- readRDS(here::here("output","Nationscape","CART_Nationscape_D1.RDS")) 
NS_CART_spec2 <- readRDS(here::here("output","Nationscape","CART_Nationscape_D2.RDS")) 
NS_CART_spec3 <- readRDS(here::here("output","Nationscape","CART_Nationscape_D3.RDS")) 
NS_CART_spec4 <- readRDS(here::here("output","Nationscape","CART_Nationscape_D4.RDS")) 

#############################
# GET PERF FOR ALL ALGORITHMS
#############################

# Small test
# test_prediction_logit <- predict.train(NS_logit__spec1, Xm_testSet_adjusted,type = "raw")
# confusionMatrix(test_prediction_logit, Xm_testSet_adjusted %>% pull(trump2Pvote_intent))

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

get_performance_stats(x = NS_logit__spec1,
                      test = Xm_testSet_adjusted,
                      depvar = "trump2Pvote_intent")


# Helper function #2 nesting the above:

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
  get_trump2p(NS1),
  get_trump2p(NS2),
  get_trump2p(NS3),
  get_trump2p(NS4),
          get_trump2p(NS_CART_spec1),
          get_trump2p(NS_CART_spec2),
          get_trump2p(NS_CART_spec3),
          get_trump2p(NS_CART_spec4),
                          get_trump2p(NS_logit__spec1),
                          get_trump2p(NS_logit__spec2),
                          get_trump2p(NS_logit__spec3),
                          get_trump2p(NS_logit__spec4),
)


NS_PERF_export <- cbind(tibble(`Model and Specification` = 
                                 c("RF: Demographics Only",
                                   "RF: Demo. + PID",
                                   "RF: Demo. + PID + Issues",
                                   "RF: All Covariates",
                                   "CART: Demographics Only",
                                   "CART: Demo. + PID",
                                   "CART: Demo. + PID + Issues",
                                   "CART: All Covariates",
                                   "Logit: Demographics Only",
                                   "Logit: Demo. + PID",
                                   "Logit: Demo. + PID + Issues",
                                   "Logit: All Covariates"
                                   )),
                          NS_PERF_TIBBLE 
)


NS_perf_RF <- NS_PERF_export[1:4,]
NS_perf_CART <- NS_PERF_export[5:8,]
NS_perf_Logit <- NS_PERF_export[9:12,]

saveRDS(NS_perf_RF,here::here("output","NS_perf_RF.RDS"))
saveRDS(NS_perf_CART,here::here("output","NS_perf_CART.RDS"))
saveRDS(NS_perf_Logit,here::here("output","NS_perf_Logit.RDS"))

xtable(NS_PERF_export, digits = 3) %>%
        print(table.placement = "H", 
              include.rownames = FALSE, 
              booktabs = TRUE,
              file = here::here("tab","NS_perf_all.tex"))




library(vip)
(VIP1 <- vip(NS1) + labs(title="Demographics Only"))
(VIP2 <- vip(NS2) + labs(title="Demo. + PID"))
(VIP3 <- vip(NS3) + labs(title="Demo. + PID + Issues"))
(VIP4 <- vip(NS4) + labs(title="All Covariates"))

ggpubr::ggarrange(VIP1,VIP2,VIP3,VIP4, align = "h")
ggsave("fig/Nationscape/NS-VIP.pdf",width=8, height=5)

####################
## ROC CURVES
####################
pdf("fig/Nationscape/NS-ranger-ROC.pdf",width=8, height=5)
par(mfrow=c(2,2))
plot(get_performance_stats(NS1,
                      test = Xm_testSet_adjusted,
                      depvar = "trump2Pvote_intent")$Perf, main = "Demographics Only")
plot(get_performance_stats(NS2,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "Demo. + PID")
plot(get_performance_stats(NS3,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "Demo. + PID + Issues")
plot(get_performance_stats(NS4,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "All Covariates")
dev.off()

pdf("fig/Nationscape/NS-CART-ROC.pdf",width=8, height=5)
par(mfrow=c(2,2))
plot(get_performance_stats(NS_CART_spec1,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "Demographics Only")
plot(get_performance_stats(NS_CART_spec2,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "Demo. + PID")
plot(get_performance_stats(NS_CART_spec3,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "Demo. + PID + Issues")
plot(get_performance_stats(NS_CART_spec4,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "All Covariates")
dev.off()

pdf("fig/Nationscape/NS-logit-ROC.pdf",width=8, height=5)
par(mfrow=c(2,2))
plot(get_performance_stats(NS_logit__spec1,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "Demographics Only")
plot(get_performance_stats(NS_logit__spec2,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "Demo. + PID")
plot(get_performance_stats(NS_logit__spec3,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "Demo. + PID + Issues")
plot(get_performance_stats(NS_logit__spec4,
                           test = Xm_testSet_adjusted,
                           depvar = "trump2Pvote_intent")$Perf, main = "All Covariates")
dev.off()


# GET "tpr", "fpr"
XVAL <- get_performance_stats(NS1,
                              test = Xm_testSet_adjusted,
                              depvar = "trump2Pvote_intent")$Perf@x.values[[1]]

YVAL <- get_performance_stats(NS1,
                              test = Xm_testSet_adjusted,
                              depvar = "trump2Pvote_intent")$Perf@y.values[[1]]

ROC_RF_Spec1 <- cbind(XVAL,YVAL) 

plot(XVAL,YVAL)


########################################################################
# Old code pulling stats step by step, incl. Brier scores...
########################################################################
# (4 specifications) estimated on the Nationscape data


get_performance_stats <- function(oos_which) {
  output <- list()
  
  output[["pred.compare"]] <- ROCR::prediction(
    oos_which$predictions[, 2], 
    as.numeric(Xm_testSet_adjusted$trump2Pvote_intent) - 1
  )
  
  output[["AUC"]] <- 
    ROCR::performance(output[["pred.compare"]], "auc")@y.values[[1]]
  output[["Perf"]] <- 
    ROCR::performance(output[["pred.compare"]], "tpr", "fpr")
  output[["Prec"]] <- 
    ROCR::performance(output[["pred.compare"]], "prec", "rec")
  output[["F1"]] <- 
    ROCR::performance(output[["pred.compare"]], "f")
  
  output
}


NS1 <- readRDS(here::here("output","Nationscape","ranger_Nationscape_D1.RDS"))
NS2 <- readRDS(here::here("output","Nationscape","ranger_Nationscape_D2.RDS"))
NS3 <- readRDS(here::here("output","Nationscape","ranger_Nationscape_D3.RDS"))
NS4 <- readRDS(here::here("output","Nationscape","ranger_Nationscape_D4.RDS"))

rf_performance_Brier <- data.frame(
  spec = c(
    "Gender, Race, Education, Income, Age (Set 1)",
    "Set 1 + Party ID (Set 2)",
    "Set 2 + Issues (Set 3)",
    "Set 3 + remaining information about respondents"
  ),
  RF = c(
    NS1$finalModel$prediction.error,
    NS2$finalModel$prediction.error,
    NS3$finalModel$prediction.error,
    NS4$finalModel$prediction.error
  )
)
rf_performance_Brier

# Now let's evaluate models one the basis of the test set,
# and calculate various OOS statistics

#############################
# CALCULATE OOS PREDICTIONS
#############################

pred_RF_OOS1 <- predict(NS1$finalModel, data = Xm_testSet_adjusted)
pred_RF_OOS2 <- predict(NS2$finalModel, data = Xm_testSet_adjusted)
pred_RF_OOS3 <- predict(NS3$finalModel, data = Xm_testSet_adjusted)
pred_RF_OOS4 <- predict(NS4$finalModel, data = Xm_testSet_adjusted)

Xm_testSet_adjusted$predicted_vote_Spec1 <- factor(
  ifelse(pred_RF_OOS1$predictions[, 2] >=
           pred_RF_OOS1$predictions[, 1], "Trump", "Biden")
)
Xm_testSet_adjusted$predicted_vote_Spec2 <- factor(
  ifelse(pred_RF_OOS2$predictions[, 2] >=
           pred_RF_OOS2$predictions[, 1], "Trump", "Biden")
)
Xm_testSet_adjusted$predicted_vote_Spec3 <- factor(
  ifelse(pred_RF_OOS3$predictions[, 2] >=
           pred_RF_OOS3$predictions[, 1], "Trump", "Biden")
)
Xm_testSet_adjusted$predicted_vote_Spec4 <- factor(
  ifelse(pred_RF_OOS4$predictions[, 2] >=
           pred_RF_OOS4$predictions[, 1], "Trump", "Biden")
)

# Specification 1 performance statistics
S1 <- confusionMatrix(
  factor(Xm_testSet_adjusted$predicted_vote_Spec1),
  factor(Xm_testSet_adjusted$trump2Pvote_intent),
  positive = "Trump"
)
# Specification 2 performance statistics
S2 <- confusionMatrix(
  factor(Xm_testSet_adjusted$predicted_vote_Spec2),
  factor(Xm_testSet_adjusted$trump2Pvote_intent),
  positive = "Trump"
)
# Specification 3 performance statistics
S3 <- confusionMatrix(
  factor(Xm_testSet_adjusted$predicted_vote_Spec3),
  factor(Xm_testSet_adjusted$trump2Pvote_intent),
  positive = "Trump"
)
# Specification 4 performance statistics
S4 <- confusionMatrix(
  factor(Xm_testSet_adjusted$predicted_vote_Spec4),
  factor(Xm_testSet_adjusted$trump2Pvote_intent),
  positive = "Trump"
)

Accuracy_Nationscape <- c(
  S1$overall[1],
  S2$overall[1],
  S3$overall[1],
  S4$overall[1]
)

F1_Nationscape <- c(
  S1$byClass[c("F1")],
  S2$byClass[c("F1")],
  S3$byClass[c("F1")],
  S4$byClass[c("F1")]
)

AUC_Nationscape <- c(
  get_performance_stats(pred_RF_OOS1)$AUC,
  get_performance_stats(pred_RF_OOS2)$AUC,
  get_performance_stats(pred_RF_OOS3)$AUC,
  get_performance_stats(pred_RF_OOS4)$AUC
)

############################
# Report Accuracy with CIs:
############################

AN <- tibble(
  what = c(
    "Demographics only",
    "Demographics + PID",
    "Demographics + PID + Issues",
    "All covariates"
  ),
  orderTib = c(1:4),
  Accuracy = c(S1$overall[1], S2$overall[1], S3$overall[1], S4$overall[1]),
  Lower = c(S1$overall[3], S2$overall[3], S3$overall[3], S4$overall[3]),
  Upper = c(S1$overall[4], S2$overall[4], S3$overall[4], S4$overall[4])
)

# AN %>%
#   ggplot(aes(x = fct_reorder(what, orderTib), y = Accuracy)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = Lower, ymax = Upper, width = .04)) +
#   labs(x = "", subtitle = "2020 vote in intent (Nationscape)") +
#   theme_minimal()
# pdf("fig/nationscape/Accuracy_2019_to2020.pdf", width = 6, height = 3)
# dev.off()
