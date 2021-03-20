##############################################
# Get important predictors of TRUMP support in 2020
##############################################

# Update: Dec. 2020
# See NS_1D_data_prep.R for latest changes

source("R/NS_0_labels.R")
source("R/NS_1D_data_prep.R")
source("R/NS_2_prep_ML.R")

library(assertthat)

set.seed(seed)

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
