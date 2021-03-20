##############################################
# Get important predictors of TRUMP support in 2020
##############################################

# Update: Dec. 2020
# See NS_1D_data_prep.R for latest changes

source("R/NS_0_labels.R")
source("R/NS_1D_data_prep.R")
source("R/NS_2_prep_ML.R")

# library(assertthat)

# ADD RELIGION
R_spec1B <- train(
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
  tuneLength = 10)

# NORTH VS SOUTH
# R_spec1C <- train(
#   trump2Pvote_intent ~ educ_category_1 +
#     educ_category_2 +
#     educ_category_3 +
#     educ_category_4 +
#     inc_group_1 + inc_group_2 +
#     inc_group_3 + inc_group_4 +
#     inc_group_5 + inc_group_NA +
#     Men_1 +
#     White_1 + Black_1 + Asian_1 + Hispanic_1 +
#     age,
#   data = Xm %>%
#     slice(D_trainsetIndex_20percent),
#   method = "ranger",
#   importance = "permutation",
#   trControl = fit_control_CV,
#   tuneLength = 10)

# DEMO. + symbolic ideology
R_spec1D <- train(
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
  tuneLength = 10)


# DEMO. + ISSUES

Xm %>%
  select(outcome,
         matches('ideo')) %>%
  head(2)










# Only education, and race
CART_Demo_MINI <- train(
  trump2Pvote_intent ~ educ_category_1 +
    educ_category_2 +
    educ_category_3 +
    educ_category_4 +
    White_1 + Black_1 + Asian_1 + Hispanic_1,
  data = Xm %>%
    slice(D_trainsetIndex_20percent),
  method = "rpart",
  trControl = fit_control_CV,
  tuneLength = 5
)

# Only education, and race
ranger_Demo_MINI <- train(
  trump2Pvote_intent ~ educ_category_1 +
    educ_category_2 +
    educ_category_3 +
    educ_category_4 +
    White_1 + Black_1 + Asian_1 + Hispanic_1,
  data = Xm %>%
    slice(D_trainsetIndex_20percent),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 5
)

# saveRDS(ranger_wImp_spec0, "output/Nationscape/ranger_Nationscape_D0.RDS")


get_performance_stats(x = CART_Demo_MINI,
                      test = Xm_testSet_adjusted,
                      depvar = "trump2Pvote_intent")
get_performance_stats(x = ranger_Demo_MINI,
                      test = Xm_testSet_adjusted,
                      depvar = "trump2Pvote_intent")


# MODEL VIZ
CART_Demo_MINI$finalModel$frame$var

library(rpart.plot)
prp(CART_Demo_MINI$finalModel,
    type=5,
    branch=.4,
    extra=104,
    under=T,
    digits = 3,
    box.palette=c("BuRd"),
    branch.col = "gray",
    branch.lwd = 2,
    fallen.leaves = TRUE,
    tweak=1.6)

