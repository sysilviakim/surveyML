# Purpose: Extract OOS accuracy and other performance metrics from RFs
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


NS1 <- readRDS("output/Nationscape/ranger_Nationscape_D1.RDS")
NS2 <- readRDS("output/Nationscape/ranger_Nationscape_D2.RDS")
NS3 <- readRDS("output/Nationscape/ranger_Nationscape_D3.RDS")
NS4 <- readRDS("output/Nationscape/ranger_Nationscape_D4.RDS")

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

source("R/NS_0_labels.R")
source("R/NS_1D_data_prep.R")
# The "ML" file is also necessary because we need the correct indices
outcome <- "trump2Pvote_intent"
source("R/NS_2_prep_ML.R")

Xm_testSet_full <- Xm[-D_trainsetIndex_20percent, ]

# We don't want a massive test set, but the necessity of this line
# hinges on what exactly we put into NS_2_prep_ML.R:
Xm_testSet_adjusted <- Xm_testSet_full %>%
  slice_sample(n = length(D_trainsetIndex_20percent) * .2)

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

AN %>%
  ggplot(aes(x = fct_reorder(what, orderTib), y = Accuracy)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, width = .04)) +
  labs(x = "", subtitle = "2020 vote in intent (Nationscape)") +
  theme_minimal()
pdf("fig/nationscape/Accuracy_2019_to2020.pdf", width = 6, height = 3)
dev.off()
