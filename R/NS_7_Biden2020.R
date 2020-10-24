######################################
# Get important predictors of BIDEN support in 2020 
# i.e. vote INTENT measured in 2019
######################################

source("NS_0_labels.R")

# We now have a new DV
source("NS_1B_data_prepForBiden.R")

source("NS_2_prep_ML.R")

library(assertthat)
# library(hrbrthemes) # useful for gg_check()
# library(mlbench)
# library(nnet)

set.seed(seed)

ranger_wImp_spec1 <- train(biden20 ~ educ_category_1 +                 
                             educ_category_2 +                
                             educ_category_3 + 
                             educ_category_4 +                
                             inc_group_1 +     
                             inc_group_2 +                                    
                             inc_group_3 +         
                             inc_group_4 +                                
                             inc_group_5 + 
                             Men + Black +
                             Asian + Hispanic +         
                             age, 
                           data=Xm[biden_trainsetIndex,],
                           method = "ranger",
                           importance = 'permutation',
                           trControl = fit_control_CV, 
                           tuneLength=10)

ranger_wImp_spec2 <- train(biden20 ~ educ_category_1 +                 
                             educ_category_2 +                
                             educ_category_3 + 
                             educ_category_4 +                
                             inc_group_1 +     
                             inc_group_2 +                                    
                             inc_group_3 +         
                             inc_group_4 +                                
                             inc_group_5 + 
                             Men + Black +
                             Asian + Hispanic +         
                             age +
                             pid7_1 +
                             pid7_2 + 
                             pid7_3 + 
                             pid7_4 +
                             pid7_5 +
                             pid7_6 + 
                             pid7_7, 
                           data=Xm[biden_trainsetIndex,],
                           method = "ranger",
                           importance = 'permutation',
                           trControl = fit_control_CV, 
                           tuneLength=10)

assert_that(names(Xm)[39]=="Jabortion_latepermit_DISAGREE"
            & names(Xm)[87] == "inc_group_5")
ranger_wImp_spec3 <- train(biden20 ~., 
                           data=Xm[biden_trainsetIndex,c(1,39:87)],
                           method = "ranger",
                           importance = 'permutation',
                           trControl = fit_control_CV, 
                           tuneLength=10)

ranger_wImp_spec4 <- train(biden20 ~., 
                            data=Xm[biden_trainsetIndex,],
                            method = "ranger",
                            importance = 'permutation',
                            trControl = fit_control_CV, 
                            tuneLength=10)




saveRDS(ranger_wImp_spec1,"../output/Nationscape/ranger_Nationscape_Biden_spec1.RDS")
saveRDS(ranger_wImp_spec2,"../output/Nationscape/ranger_Nationscape_Biden_spec2.RDS")
saveRDS(ranger_wImp_spec3,"../output/Nationscape/ranger_Nationscape_Biden_spec3.RDS")
saveRDS(ranger_wImp_spec4,"../output/Nationscape/ranger_Nationscape_Biden_spec4.RDS")


importance(ranger_wImp_spec4$finalModel)

# CAUTION: This will take hours 
### imp_frame4 <- measure_importance(ranger_wImp_spec4$finalModel)
# saveRDS(imp_frame4,"../output/Nationscape/ranger_importanceframe_Nationscape_Biden_spec4.RDS")

# Load saved importance frame of specification 4:
imp_frame4 <- readRDS("../output/Nationscape/ranger_importanceframe_Nationscape_Biden_spec4.RDS")

most_imp_var_2criteria <- (vars <- important_variables(imp_frame4, k = 5, measures = c("mean_min_depth", "times_a_root")))

# not working:
# interactions_frame <- min_depth_interactions(ranger_wImp_spec4, most_imp_var_2criteria)

# plot_min_depth_distribution(imp_frame4$mean_min_depth)

plot_importance_ggpairs(imp_frame4, measures = c("mean_min_depth", "times_a_root"))

# importance(fit_ranger_withImp$finalModel)
# importance_pvalues(fit_ranger_withImp$finalModel) # this is not quite right
# imp_frame <- measure_importance(fit_ranger_withImp$finalModel)
# 
# fit_ranger_withImp$finalModel$confusion.matrix
# 
# fit_ranger_withImp$results


# RuleFit / PRE models
# The RuleFit algorithm by Friedman and Popescu (2008) learns sparse linear models that include automatically detected interaction effects in the form of decision rules.

# no tuning 1,3,4,5,6,7,8,9,10 11 13 ...
# library(pre)
# pre.biden <- pre(biden20 ~ ., data = Xm[biden_trainsetIndex,])

# library(xrf)
# rulefit.biden <- xrf(biden20 ~ ., 
#                      data = as.data.frame(Xm[biden_trainsetIndex,]),
#                      family = 'binomial')



# use caret
preFIT4 <- train(biden20 ~., 
                 data=Xm[biden_trainsetIndex,],
                 method = "caret_pre_model",
                 trControl = fit_control_CV)
