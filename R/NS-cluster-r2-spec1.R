source("R/NS_0_labels.R")

# We now have a new DV
source("R/NS_1C_data_prepForBiden_Release2.R")

source("R/NS_2_prep_ML.R")

ranger_wImp_spec1 <- train(
  biden20 ~ educ_category_1 +
    educ_category_2 +
    educ_category_3 +
    educ_category_4 +
    household_income_1 + household_income_2 +
    household_income_3 + household_income_4 +
    household_income_5 + household_income_6 +
    household_income_7 + household_income_8 +
    household_income_9 + household_income_10 +
    household_income_11 + household_income_12 +
    household_income_13 + household_income_14 +
    household_income_15 + household_income_16 +
    household_income_17 + household_income_18 +
    household_income_19 + household_income_20 +
    household_income_21 + household_income_22 +
    household_income_23 + household_income_24 +
    Men + White + Black +
    Asian + Hispanic +
    age,
  data = Xm %>%
    slice(biden_trainsetIndex),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10
)

saveRDS(
  ranger_wImp_spec1,
  "CLUSTER_Nationscape_Biden_R2_spec1_MARCH-to-JUNE2020v2.RDS"
)
