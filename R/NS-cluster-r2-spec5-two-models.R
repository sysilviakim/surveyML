source("NS_0_labels.R")

# We now have a new DV
source("NS_1C_data_prepForBiden_Release2.R")

source("NS_2_prep_ML.R")


ranger_wImp_spec5_symbolic_ideo <- train(biden20 ~ educ_category_1 +                 
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
                                         age +
                                         pid7_legacy_1 +
                                         pid7_legacy_2 + 
                                         pid7_legacy_3 + 
                                         pid7_legacy_4 +
                                         pid7_legacy_5 +
                                         pid7_legacy_6 + 
                                         pid7_legacy_7 +
                                           ideo5_1 + ideo5_2 + ideo5_3 + ideo5_4 + ideo5_5,             
                           data=Xm[biden_trainsetIndex,],
                           method = "ranger",
                           importance = 'permutation',
                           trControl = fit_control_CV, 
                           tuneLength=10)

saveRDS(ranger_wImp_spec5_symbolic_ideo,"CLUSTER_Nationscape_Biden_R2_spec5_SYMBOLILC_MARCH-to-JUNE2020v2.RDS")



### AND NOW TAKE OPERATIONAL IDEOLOGY

a_analysis <- a_release2 %>% filter(date >= "2020-03-04",
                                    registered==1,
                                    !is.na(trump_biden),
                                    trump_biden != 999) %>%    # dropping undecided voters
  select(outcome,
         monthyear,
         all_of(gender_vars),
         all_of(racial_att_vars),
         all_of(group_attitudes),
         culture_full_theta_by1SD,
         econ_cons_theta_by1SD,
         immig_full_theta_by1SD,
         White,
         Men,Black,Asian,
         Hispanic,
         age,
         pid7_legacy,
         ideo5,
         educ_category,
         household_income,
         household_gun_owner,
         in_union,
         is_evangelical,
         religion,
         language,
         orientation_group,
         sc_knowledge,
         sen_knowledge)


names(a_analysis)
# One-hot encoding
# SUBSETTING IS FRAGILE as we are relying here on the correct order of columns...
a_analysis[,which(names(a_analysis)=="pid7_legacy"):which(names(a_analysis)=="sen_knowledge")] <- lapply(a_analysis[,which(names(a_analysis)=="pid7_legacy"):which(names(a_analysis)=="sen_knowledge")] , factor)
Xm1 <- dummy_cols(a_analysis,remove_selected_columns=T)
Xm <- Xm1 %>% filter(complete.cases(.))

# This must come AFTER one-hot encoding the features,
# because we don't want to have two separate outcome variables: just keep a single binary variable.
Xm$biden20 <- factor(Xm$biden20, levels = c(0,1), labels = c("Trump", "Biden"))


source("NS_2_prep_ML.R")

ranger_wImp_spec5_operational_ideo <- train(biden20 ~ educ_category_1 +                 
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
                                           age +
                                           pid7_legacy_1 +
                                           pid7_legacy_2 + 
                                           pid7_legacy_3 + 
                                           pid7_legacy_4 +
                                           pid7_legacy_5 +
                                           pid7_legacy_6 + 
                                           pid7_legacy_7 +
                                          culture_full_theta_by1SD + 
                                         econ_cons_theta_by1SD + 
                                         immig_full_theta_by1SD,            
                                         data=Xm[biden_trainsetIndex,],
                                         method = "ranger",
                                         importance = 'permutation',
                                         trControl = fit_control_CV, 
                                         tuneLength=10)

saveRDS(ranger_wImp_spec5_operational_ideo,"CLUSTER_Nationscape_Biden_R2_spec5_OPERATIONAL_MARCH-to-JUNE2020v2.RDS")

