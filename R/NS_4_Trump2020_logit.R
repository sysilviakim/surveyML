# See NS_1D_data_prep.R for latest changes

source("R/NS_0_labels.R")
source("R/NS_1D_data_prep.R")
source("R/NS_2_prep_ML.R")

library(assertthat)

set.seed(seed)

# Only education, gender, age, and race [so dropping income]
NS_logit__spec0 <- train(
  trump2Pvote_intent ~ educ_category_1 +
    educ_category_2 +
    educ_category_3 +
    educ_category_4 +
    Men_1 +
    White_1 + Black_1 + Asian_1 + Hispanic_1 +
    age,
  data = Xm %>%
    slice(D_trainsetIndex_20percent),
  method = "glm",
  family = "binomial",
  trControl = fit_control_CV,
  tuneLength = 5
)

NS_logit__spec1 <- train(
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
  method = "glm",
  family = "binomial",
  trControl = fit_control_CV,
  tuneLength = 10
)

NS_logit__spec2 <- train(
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
  method = "glm",
  family = "binomial",
  trControl = fit_control_CV,
  tuneLength = 10
)

NS_logit__spec3 <- train(trump2Pvote_intent ~ .,
  data = Xm %>%
    slice(D_trainsetIndex_20percent) %>%
    select(Jabortion_latepermit_DISAGREE_0:inc_group_NA, outcome),
  method = "glm",
  family = "binomial",
  trControl = fit_control_CV,
  tuneLength = 10
)

NS_logit__spec4 <- train(trump2Pvote_intent ~ .,
  data = Xm[D_trainsetIndex_20percent, ],
  method = "glm",
  family = "binomial",
  trControl = fit_control_CV,
  tuneLength = 10
)


saveRDS(NS_logit__spec0, "output/Nationscape/logit_Nationscape_D0.RDS")
saveRDS(NS_logit__spec1, "output/Nationscape/logit_Nationscape_D1.RDS")
saveRDS(NS_logit__spec2, "output/Nationscape/logit_Nationscape_D2.RDS")
saveRDS(NS_logit__spec3, "output/Nationscape/logit_Nationscape_D3.RDS")
saveRDS(NS_logit__spec4, "output/Nationscape/logit_Nationscape_D4.RDS")

