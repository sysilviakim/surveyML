###############
# ML via CARET
###############

source("NS_0_labels.R")
source("NS_1C_data_prepForBiden_Release2.R")
source("NS_2_prep_ML.R")


# Release 1, modelling trump16

fit_rpart <- train(trump16 ~., 
                   data=Xm,
                   subset = Xm_trainset_indices,
                   method = "rpart",
                   trControl = fit_control,
                   tuneLength=10
)

fit_cforest <- train(trump16 ~., 
                   data=Xm,
                   subset = Xm_trainset_indices,
                   method = "cforest",
                   trControl = fit_control,
                   tuneLength=10
                   )

#saveRDS(fit_rpart,"../output/fit_rpart_NS_July25_2020.RDS")

# Add labels [run this twice]
fit_rpart$finalModel$frame$var <- relaber(fit_rpart$finalModel$frame$var)

prp(fit_rpart$finalModel,
    type=5,
    branch=.4,
    extra=104,
    under=T,
    digits = 3,
    box.palette=c("BuRd"),
    branch.col = "gray",
    branch.lwd = 2,
    fallen.leaves = TRUE,
    tweak=1.5,
    under.cex=.65)
dev.copy(pdf,"NS_rpart_trump16_tuned_july25_TT.pdf", width=10, height=6)
dev.off()

names(fit_rpart$finalModel$variable.importance) <- relaber(names(fit_rpart$finalModel$variable.importance))
dotchart(fit_rpart$finalModel$variable.importance[15:1],pch=16,xlab = "Variable importance",main="Top 15 features")
dev.copy(pdf,"NS_rpart_trump16_tuned_VIP_july25_TT.pdf",width=10, height=6)
dev.off()

summary(fit_rpart$finalModel)



# Release 2, modeling biden20 after SuperTuesday...

Biden_rpart <- train(biden20 ~., 
                   data=Xm,
                   subset = as.vector(biden_trainsetIndex),
                   method = "rpart",
                   trControl = fit_control_CV,
                   tuneLength=10)

saveRDS(Biden_rpart,"../output/Nationscape/Biden_rpart_NS_R2_Oct5_2020.RDS")

# Add labels [run this twice]
Biden_rpart$finalModel$frame$var <- relaber(Biden_rpart$finalModel$frame$var)
Biden_rpart$finalModel$frame$var <- relab2(Biden_rpart$finalModel$frame$var)
prp(Biden_rpart$finalModel,
    type=5,
    branch=.4,
    extra=104,
    under=F,
    digits = 3,
    box.palette=c("RdBu"),
    branch.col = "gray",
    branch.lwd = 4,
    fallen.leaves = F,
    tweak=1.1) # under.cex=.9)
dev.copy(pdf,"../output/Nationscape/NS_R2_rpart_biden20_tuned_oct5.pdf", width=10, height=6)
dev.off()

names(Biden_rpart$finalModel$variable.importance) <- relaber(names(Biden_rpart$finalModel$variable.importance))
names(Biden_rpart$finalModel$variable.importance) <- relab2(names(Biden_rpart$finalModel$variable.importance))
dotchart(Biden_rpart$finalModel$variable.importance[15:1],pch=16,xlab = "Variable importance",main="Top 15 features")
dev.copy(pdf,"NS_R2_rpart_biden20_tuned_VIP_oct5.pdf",width=10, height=6)
dev.off()

summary(Biden_rpart$finalModel)


# We now estimate RFs in a separate file
# 
# 
#  fit_ranger_withImp <- train(trump16 ~., 
#                  data=Xm,
#                  subset = Xm_trainset_indices,
#                  method = "ranger",
#                  importance = 'permutation',
#                  trControl = fit_control_basic, # This was previous excecuted with fit_control_basic
#                  tuneLength=10,
#  )
#  
# saveRDS(fit_ranger_withImp,"fit_ranger_withImp_July25_2020.RDS")
#  
# 
# 
# importance(fit_ranger_withImp$finalModel)
# importance_pvalues(fit_ranger_withImp$finalModel) # this is not quite right
# imp_frame <- measure_importance(fit_ranger_withImp$finalModel)
# 
# fit_ranger_withImp$finalModel$confusion.matrix
# 
# fit_ranger_withImp$results
# 
# 
# # EVALUATE MODELS ON THE HOLDOUT (TEST) SET
# # twoClassSummary(Xm_testset_data, lev = levels(Xm_testset_data$trump16))
# # prSummary(Xm_testset_data)
# 
# # library(MLeval)
# # rf_mevav <- evalm(fit_ranger_withImp)
# # ## get roc curve plotted in ggplot2
# # rf_mevav$roc
# # ## get AUC and other metrics
# # rf_mevav$stdres
# 
# nrow(Xm_trainset)
# nrow(Xm[-Xm_trainset,])
# nrow(Xm)
# 
# pred_data_ranger <- predict(fit_ranger_withImp$finalModel, Xm_testset_data)
# pred_df_ranger <- cbind(pred_data_ranger$predictions, actual_vote = Xm_testset_data[,"trump16"]) 
# 
# # Basic confusion matrix
# table(pred_df_ranger)
# # Rich statistics
# confusionMatrix(pred.data_ranger$predictions,pred_df_ranger$trump16)
# 
# 
# pred_data_caret <- predict(fit_rpart$finalModel, newdata = Xm_testset_data,type="class")
# pred_df_caret <- cbind(pred_data_caret,actual_Vote=Xm_testset_data[,"trump16"])
# # Basic confusion matrix
# table(pred_df_caret)
# # Rich statistics
# confusionMatrix(pred_data_caret,pred_df_caret$trump16)
# 
# 
# ###################
# # RANGER IMPORTANCE
# ###################
# 
# # Prepare a table for plotting the most important variables:
# XT <- tibble(VAR=names(fit_ranger_withImp$finalModel$variable.importance),
#              I=fit_ranger_withImp$finalModel$variable.importance)
# 
# # Prepare labels
# XT$VAR <- relaber(XT$VAR)
# 
# XT %>% 
#   dplyr::arrange(desc(I)) %>%
#   dplyr::top_n(20) %>%
#   ggplot(aes(reorder(VAR, I), I)) +
#   geom_col() +
#   coord_flip() +
#   ggtitle("Top 20 predictors of 2016 vote choice") +
#   labs(y="Increase in mean squared error in the event of variable permutation", x="Inputs into the random forest")
# dev.copy(pdf,"NS_ranger_tuned_july10.pdf",width=10, height=6)
# dev.off()
# 
# 
# # Comparing models: approach 1:
# predictions <- extractPrediction(
#   models = list(fit_rpart, fit_ranger_withImp),
#   testX = Xm_testset_data
#   #testY = Xm_testset_data$trump16
# )
# 
# summary(predictions)
# 



