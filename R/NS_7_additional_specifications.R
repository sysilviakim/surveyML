##############################################
# Get important predictors of TRUMP support in 2020
##############################################

# This will create a new "Xm_GEO" dataset with a "South" dummy.
geo_analysis <- T

source("R/NS_0_labels.R")
source("R/NS_1D_data_prep.R")
source("R/NS_2_prep_ML.R")

demos <- c(
  "educ_category_1",
  "educ_category_2",
  "educ_category_3",
  "educ_category_4",
  "inc_group_1",
  "inc_group_2",
  "inc_group_3",
  "inc_group_4",
  "inc_group_5",
  "inc_group_NA",
  "Men_1",
  "White_1",
  "Black_1",
  "Asian_1",
  "Hispanic_1",
  "age"
)

Xm$Liberal <- ifelse(Xm$ideo5_1==1 | Xm$ideo5_2==1, 1, 0)
Xm$Moderate <- ifelse(Xm$ideo5_3, 1, 0)
Xm$Conservative <- ifelse(Xm$ideo5_4==1 | Xm$ideo5_5==1, 1, 0)

############################
# Demographics +  religion
############################
R_spec1B <- train(
  trump2Pvote_intent ~ .,
  data = Xm %>%
    select(outcome,
           demos,
           matches("is_evangelical")) %>%
    slice(D_trainsetIndex_20percent),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10)

saveRDS(R_spec1B, "output/Nationscape/R_spec1B.RDS")


################
#NORTH VS SOUTH
################
R_spec1C <- train(trump2Pvote_intent ~ .,
  data = Xm_GEO %>%
    slice(D_trainsetIndex_20percent),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10)

saveRDS(R_spec1C, "output/Nationscape/R_spec1C.RDS")

################################
# DEMO. + symbolic ideology
################################
R_spec1D <- train(
  trump2Pvote_intent ~ .,
  data = Xm %>%
    select(outcome,
           demos,
           Conservative,
           Moderate,
           Liberal) %>%
    slice(D_trainsetIndex_20percent),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10)

saveRDS(R_spec1D, "output/Nationscape/R_spec1D.RDS")

################################################
# DEMO. + ISSUES [run on the Cluster]
################################################
R_spec1E <- train(
  trump2Pvote_intent ~ .,
  data = Xm %>%
    select(outcome,
           demos,
           Jdeportation_agree_0:Jabortion_latepermit_DISAGREE_0) %>%
    slice(D_trainsetIndex_20percent),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10)






#############
# CART MODELS
#############
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

CART_spec1D <- train(
  trump2Pvote_intent ~ .,
  data = Xm %>%
    select(outcome,
           demos,
           Conservative,
           Moderate,
           Liberal) %>%
    slice(D_trainsetIndex_20percent),
  method = "rpart",
  trControl = fit_control_CV,
  tuneLength = 10)


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

prp(CART_spec1D$finalModel,
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

