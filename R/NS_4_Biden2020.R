######################################
# Get important predictors of TRUMP support in 2020
######################################

# Update: Dec. 2020
# See NS_1D_data_prep.R for latest changes

source("NS_0_labels.R")
source("NS_1D_data_prep.R")
source("NS_2_prep_ML.R")

library(assertthat)

set.seed(seed)

# Only education, gender, age, and race
ranger_wImp_spec0 <- train(
  trump2Pvote_intent ~ educ_category_1 +
    educ_category_2 +
    educ_category_3 +
    educ_category_4 +
    Men_1 +
    White_1 + Black_1 + Asian_1 + Hispanic_1 +
    age,
  data = Xm %>%
    slice(D_trainsetIndex_20percent),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 5
)

ranger_wImp_spec1 <- train(
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
  tuneLength = 10
)

ranger_wImp_spec2 <- train(
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
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10
)

ranger_wImp_spec3 <- train(trump2Pvote_intent ~ .,
  data = Xm %>%
    slice(D_trainsetIndex_20percent) %>%
    select(Jabortion_latepermit_DISAGREE_0:inc_group_NA, outcome),
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10
)

ranger_wImp_spec4 <- train(trump2Pvote_intent ~ .,
  data = Xm[D_trainsetIndex_20percent, ],
  method = "ranger",
  importance = "permutation",
  trControl = fit_control_CV,
  tuneLength = 10
)

logit_spec4 <- glm(trump2Pvote_intent ~ .,
  data = Xm[D_trainsetIndex_20percent, ],
  family = "binomial"
)

saveRDS(ranger_wImp_spec0, "output/Nationscape/ranger_Nationscape_D0.RDS")
saveRDS(ranger_wImp_spec1, "output/Nationscape/ranger_Nationscape_D1.RDS")
saveRDS(ranger_wImp_spec2, "output/Nationscape/ranger_Nationscape_D2.RDS")
saveRDS(ranger_wImp_spec3, "output/Nationscape/ranger_Nationscape_D3.RDS")
saveRDS(ranger_wImp_spec4, "output/Nationscape/ranger_Nationscape_D4.RDS")

# Can work on the stuff below:

library(randomForestExplainer)

# we tried "accuracy_decrease" but it did not show up...
# is it the same as "permutation"?
# imp_frame3_R2_v2 <- measure_importance(
#   ranger_wImp_spec3$finalModel,
#   measures = c("permutation", "times_a_root")
# )
# saveRDS(
#   imp_frame3_R2_v2,
#   "output/Nationscape/ranger_importanceframe_Nationscape_Biden_spec3_R2_v2.RDS"
# )

imp_frame3_R2_v2 <- readRDS(
  imp_frame3_R2_v2,
  "output/Nationscape/ranger_importanceframe_Nationscape_Biden_spec3_R2_v2.RDS"
)

imp_frame3_R2_v2$variable <- relaber(imp_frame3_R2_v2$variable)
imp_frame3_R2_v2$variable <- relab2(imp_frame3_R2_v2$variable)
imp_frame3_R2_v2$variable <- plyr::revalue(
  imp_frame3_R2_v2$variable,
  c("Jpath4all_DISAGREE" = "Against path to citizenship")
)
imp_frame3_R2_v2$variable <- plyr::revalue(
  imp_frame3_R2_v2$variable, c("Very\tConservative" = "Very conservative")
)

sum(imp_frame3_R2_v2$times_a_root)

imp_frame3_R2_v2 %>%
  mutate(root = (times_a_root / 500) * 100) %>%
  arrange(desc(root)) %>%
  slice(1:10) %>%
  ggdotchart(
    x = "variable", y = "times_a_root",
    fill = "grey",
    sorting = "descending",
    add = "segments",
    ylab = "Times root node (out of 500)",
    xlab = "",
    title = "Predicting Biden vote intent\n(with demographics + PID + issues)",
    rotate = TRUE,
    ggtheme = theme_minimal()
  )
ggsave("fig/Nationscape/imp_frame3_R2_v2_rootnode.pdf")

#  ggplot(aes(y=variable,x=root)) + geom_bar(stat = "identity")

library(randomForestExplainer)
imp_frame1 <- measure_importance(ranger_wImp_spec1$finalModel)

# CAUTION: This will take hours
# imp_frame4 <- measure_importance(ranger_wImp_spec4$finalModel)
# saveRDS(
#   imp_frame4,
#   "output/Nationscape/ranger_importanceframe_Nationscape_Biden_spec4.RDS"
# )

# Load saved importance frame of specification 4:
imp_frame4 <- readRDS(
  "output/Nationscape/ranger_importanceframe_Nationscape_Biden_spec4.RDS"
)

most_imp_var_2criteria <-
  (vars <- important_variables(
    imp_frame4,
    k = 5, measures = c("mean_min_depth", "times_a_root")
  ))

# not working:
# interactions_frame <- 
#   min_depth_interactions(ranger_wImp_spec4, most_imp_var_2criteria)

# plot_min_depth_distribution(imp_frame4$mean_min_depth)

plot_importance_ggpairs(
  imp_frame4, measures = c("mean_min_depth", "times_a_root")
)

# importance(fit_ranger_withImp$finalModel)
# importance_pvalues(fit_ranger_withImp$finalModel) # this is not quite right
# imp_frame <- measure_importance(fit_ranger_withImp$finalModel)
#
# fit_ranger_withImp$finalModel$confusion.matrix
#
# fit_ranger_withImp$results
