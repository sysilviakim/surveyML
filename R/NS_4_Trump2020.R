####################################################
# Get important predictors of TRUMP support in 2020
###################################################

# Update: Dec. 2020
# See NS_1D_data_prep.R for latest changes

source("R/NS_0_labels.R")
source("R/NS_1D_data_prep.R")
source("R/NS_2_prep_ML.R")

library(assertthat)

set.seed(seed)

# Only education, gender, age, and race
ranger_wImp_spec0 <- train(
  trump2Pvote_intent ~ educ_category_1 +
    educ_category_2 +
    educ_category_3 +
    educ_category_4 +
    Men_1 +
    White_1 + Black_1 + Asian_1 + Hispanic_1 +
    age,
  data = Xm %>%
    slice(D_trainsetIndex_20percent),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 5
)

ranger_wImp_spec1 <- train(
  trump2Pvote_intent ~ educ_category_1 +
    educ_category_2 +
    educ_category_3 +
    educ_category_4 +
    inc_group_1 + inc_group_2 +
    inc_group_3 + inc_group_4 +
    inc_group_5 + inc_group_NA +
    Men_1 +
    White_1 + Black_1 + Asian_1 + Hispanic_1 +
    age,
  data = Xm %>%
    slice(D_trainsetIndex_20percent),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10
)

ranger_wImp_spec2 <- train(
  trump2Pvote_intent ~ educ_category_1 +
    educ_category_2 +
    educ_category_3 +
    educ_category_4 +
    inc_group_1 + inc_group_2 +
    inc_group_3 + inc_group_4 +
    inc_group_5 + inc_group_NA +
    Men_1 +
    White_1 + Black_1 + Asian_1 + Hispanic_1 +
    age +
    pid7_legacy_1 +
    pid7_legacy_2 +
    pid7_legacy_3 +
    pid7_legacy_4 +
    pid7_legacy_5 +
    pid7_legacy_6 +
    pid7_legacy_7,
  data = Xm %>%
    slice(D_trainsetIndex_20percent),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10
)

ranger_wImp_spec3 <- train(trump2Pvote_intent ~ .,
  data = Xm %>%
    slice(D_trainsetIndex_20percent) %>%
    select(Jabortion_latepermit_DISAGREE_0:inc_group_NA, outcome),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10
)

ranger_wImp_spec4 <- train(trump2Pvote_intent ~ .,
  data = Xm[D_trainsetIndex_20percent, ],
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10
)

logit_spec4 <- glm(trump2Pvote_intent ~ .,
  data = Xm[D_trainsetIndex_20percent, ],
  family = "binomial"
)

saveRDS(ranger_wImp_spec0, "output/Nationscape/ranger_Nationscape_D0.RDS")
saveRDS(ranger_wImp_spec1, "output/Nationscape/ranger_Nationscape_D1.RDS")
saveRDS(ranger_wImp_spec2, "output/Nationscape/ranger_Nationscape_D2.RDS")
saveRDS(ranger_wImp_spec3, "output/Nationscape/ranger_Nationscape_D3.RDS")
saveRDS(ranger_wImp_spec4, "output/Nationscape/ranger_Nationscape_D4.RDS")

