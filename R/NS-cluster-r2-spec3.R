source("NS_0_labels.R")

# We now have a new DV
source("NS_1C_data_prepForBiden_Release2.R")

source("NS_2_prep_ML.R")

library(assertthat)

assert_that(names(Xm)[40]=="Jabortion_latepermit_DISAGREE"
            & names(Xm)[108] == "household_income_24")

ranger_wImp_spec3 <- train(biden20 ~., 
                           data= Xm  %>% 
                             slice(biden_trainsetIndex) %>%
                             select(1,Jabortion_latepermit_DISAGREE:household_income_24),
                           method = "ranger",
                           importance = 'permutation',
                           trControl = fit_control_CV, 
                           tuneLength=10)

saveRDS(ranger_wImp_spec3,"CLUSTER_Nationscape_Biden_R2_spec3_MARCH-to-JUNE2020v2.RDS")
