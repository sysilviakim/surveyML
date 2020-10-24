######################################
# Get important predictors of BIDEN support in 2020 
# i.e. vote INTENT measured in in 2020
######################################

source("NS_0_labels.R")

# We now have a new DV
source("NS_1C_data_prepForBiden_Release2.R")

source("NS_2_prep_ML.R")

library(assertthat)
# library(hrbrthemes) # useful for gg_check()
# library(mlbench)
# library(nnet)


# TO DO:
### [DONE] BROADER INCOME GROUP: household_income
### ! Spec 3/4: Need a version Just use ISSUE scales [otherwise issues compete against each otehr]

### [YES] are undecided voters dropped??
### [YES] can you try the cluster?


set.seed(seed)

# Only education, gender, age, and race
ranger_wImp_spec0 <- train(biden20 ~ educ_category_1 +                 
                             educ_category_2 +                
                             educ_category_3 + 
                             educ_category_4 +  
                             Men + 
                             White + Black + Asian + Hispanic +         
                             age, 
                           data= Xm  %>% 
                             slice(biden_trainsetIndex),
                           method = "ranger",
                           importance = 'permutation',
                           trControl = fit_control_CV, 
                           tuneLength=5)

ranger_wImp_spec1 <- train(biden20 ~ educ_category_1 +                 
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
                             Men + 
                             White + Black + Asian + Hispanic +         
                             age, 
                           data= Xm  %>% 
                             slice(biden_trainsetIndex),
                           method = "ranger",
                           importance = 'permutation',
                           trControl = fit_control_CV, 
                           tuneLength=10)

# 10/3/2020: putting specs 2, 3, 4 on the NYU cluster

ranger_wImp_spec2 <- train(biden20 ~ educ_category_1 +                 
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
                             Men + 
                             White + Black + Asian + Hispanic +         
                             age +
                             pid7_legacy_1 +
                             pid7_legacy_2 + 
                             pid7_legacy_3 + 
                             pid7_legacy_4 +
                             pid7_legacy_5 +
                             pid7_legacy_6 + 
                             pid7_legacy_7, 
                           data= Xm  %>% 
                             slice(biden_trainsetIndex),
                           method = "ranger",
                           importance = 'permutation',
                           trControl = fit_control_CV, 
                           tuneLength=10)

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

ranger_wImp_spec4 <- train(biden20 ~., 
                            data=Xm[biden_trainsetIndex,],
                            method = "ranger",
                            importance = 'permutation',
                            trControl = fit_control_CV, 
                            tuneLength=10)

logit_spec4 <- glm(biden20 ~., 
                           data=Xm[biden_trainsetIndex,],
                   family = "binomial")


saveRDS(ranger_wImp_spec0,"../output/Nationscape/ranger_Nationscape_Biden_R2_spec0_MARCH-to-JUNE2020v2.RDS")
saveRDS(ranger_wImp_spec1,"../output/Nationscape/ranger_Nationscape_Biden_R2_spec1_MARCH-to-JUNE2020.RDS")
saveRDS(ranger_wImp_spec2,"../output/Nationscape/ranger_Nationscape_Biden_R2_spec2_MARCH-to-JUNE2020.RDS")
saveRDS(ranger_wImp_spec3,"../output/Nationscape/ranger_Nationscape_Biden_R2_spec3_MARCH-to-JUNE2020.RDS")
saveRDS(ranger_wImp_spec4,"../output/Nationscape/ranger_Nationscape_Biden_R2_spec4_MARCH-to-JUNE2020.RDS")


library(randomForestExplainer)

# we tried "accuracy_decrease" but it did not show up... is it the same as "permutation"?
#imp_frame3_R2_v2 <- measure_importance(ranger_wImp_spec3$finalModel, measures = c("permutation","times_a_root"))
#saveRDS(imp_frame3_R2_v2,"../output/Nationscape/ranger_importanceframe_Nationscape_Biden_spec3_R2_v2.RDS")

imp_frame3_R2_v2 <- readRDS(imp_frame3_R2_v2,"../output/Nationscape/ranger_importanceframe_Nationscape_Biden_spec3_R2_v2.RDS")

imp_frame3_R2_v2$variable <- relaber(imp_frame3_R2_v2$variable)
imp_frame3_R2_v2$variable <- relab2(imp_frame3_R2_v2$variable)
imp_frame3_R2_v2$variable <- plyr::revalue(imp_frame3_R2_v2$variable, c("Jpath4all_DISAGREE" = "Against path to citizenship"))
imp_frame3_R2_v2$variable <- plyr::revalue(imp_frame3_R2_v2$variable, c("Very\tConservative" = "Very conservative"))

sum(imp_frame3_R2_v2$times_a_root)

imp_frame3_R2_v2 %>% mutate(root = (times_a_root/500)*100) %>%
  arrange(desc(root)) %>% slice(1:10) %>%
  ggdotchart(x = "variable", y = "times_a_root",
            fill = "grey",
            sorting =  "descending",
            add = "segments",
            ylab = "Times root node (out of 500)",
            xlab = "",
            title = "Predicting Biden vote intent\n(with demographics + PID + issues)",
            rotate = TRUE,
            ggtheme = theme_minimal()
  )
ggsave("../fig/Nationscape/imp_frame3_R2_v2_rootnode.pdf")
  
#  ggplot(aes(y=variable,x=root)) + geom_bar(stat = "identity")

library(randomForestExplainer)
imp_frame1 <- measure_importance(ranger_wImp_spec1$finalModel)
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
