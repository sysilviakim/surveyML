source("R/NS_0_labels.R")

# We now have a new DV
source("R/NS_1C_data_prepForBiden_Release2.R")

source("R/NS_2_prep_ML.R")


ranger_wImp_spec4 <- train(
  biden20 ~ .,
  data = Xm[biden_trainsetIndex, ],
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 5
)

saveRDS(
  ranger_wImp_spec4,
  "CLUSTER_Nationscape_Biden_R2_spec4_MARCH-to-JUNE2020v2.RDS"
)
