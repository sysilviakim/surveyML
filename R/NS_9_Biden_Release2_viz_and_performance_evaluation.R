library(cvms) # plot_confusion_matrix
library(ggpubr) # ggdensity()
library(pROC)
library(plotROC)
library(vip)
library(tidyverse)

source("NS_0_labels.R")
# For computing OOS predictions, we need the dataset of all respondents:
source("NS_1C_data_prepForBiden_Release2.R")

# Read in the models:
ranger_wImp_spec0 <- readRDS("../output/Nationscape/ranger_Nationscape_Biden_R2_spec0_MARCH-to-JUNE2020v2.RDS")
ranger_wImp_spec1 <- readRDS("../output/Nationscape/CLUSTER_Nationscape_Biden_R2_spec1_MARCH-to-JUNE2020v2.RDS")
ranger_wImp_spec2 <- readRDS("../output/Nationscape/CLUSTER_Nationscape_Biden_R2_spec2_MARCH-to-JUNE2020v2.RDS")
ranger_wImp_spec3 <- readRDS("../output/Nationscape/CLUSTER_Nationscape_Biden_R2_spec3_MARCH-to-JUNE2020v2.RDS")
ranger_wImp_spec4 <- readRDS("../output/Nationscape/CLUSTER_Nationscape_Biden_R2_spec4_MARCH-to-JUNE2020v2.RDS")
# This one useed white_noncollege instead of White
#ranger_wImp_spec4 <- readRDS("../output/Nationscape/CLUSTER_Nationscape_Biden_R2_spec4_MARCH-to-JUNE2020.RDS")


# Rename variables:
names(ranger_wImp_spec0$finalModel$variable.importance) <- relaber(names(ranger_wImp_spec0$finalModel$variable.importance))
names(ranger_wImp_spec0$finalModel$variable.importance) <- relab2(names(ranger_wImp_spec0$finalModel$variable.importance))

names(ranger_wImp_spec1$finalModel$variable.importance) <- relaber(names(ranger_wImp_spec1$finalModel$variable.importance))
names(ranger_wImp_spec1$finalModel$variable.importance) <- relab2(names(ranger_wImp_spec1$finalModel$variable.importance))

# Sometimes 2 relabers are needed; specifically, adding relab2()
names(ranger_wImp_spec2$finalModel$variable.importance) <- relaber(names(ranger_wImp_spec2$finalModel$variable.importance))
names(ranger_wImp_spec2$finalModel$variable.importance) <- relab2(names(ranger_wImp_spec2$finalModel$variable.importance))

names(ranger_wImp_spec3$finalModel$variable.importance) <- relaber(names(ranger_wImp_spec3$finalModel$variable.importance))
names(ranger_wImp_spec3$finalModel$variable.importance) <- relab2(names(ranger_wImp_spec3$finalModel$variable.importance))

names(ranger_wImp_spec4$finalModel$variable.importance) <- relaber(names(ranger_wImp_spec4$finalModel$variable.importance))
names(ranger_wImp_spec4$finalModel$variable.importance) <- relab2(names(ranger_wImp_spec4$finalModel$variable.importance))
  
   


(v1 <- vip(ranger_wImp_spec1$finalModel,num_features = 12) +
  labs(subtitle="1: Gender, Race, Educ., Income, Age") )
#title = "Predicting Biden support (Nov. 2020 vote intent) between March and June 2020",
ggsave("../fig/Nationscape/R2_RF_BIDEN2020_VIP_spec1_onlySubtitle.pdf")

(v2 <- vip(ranger_wImp_spec2$finalModel,num_features = 12) +
  labs(subtitle="2: G., R., E., I., A. + Partisanship") )
ggsave("../fig/Nationscape/R2_RF_BIDEN2020_VIP_spec2_onlySubtitle.pdf")

(v3 <- vip(ranger_wImp_spec3$finalModel,num_features = 12) +
  labs(subtitle="3: All above + Issues") )
ggsave("../fig/Nationscape/R2_RF_BIDEN2020_VIP_spec3_onlySubtitle.pdf")

(v4 <- vip(ranger_wImp_spec4$finalModel,num_features = 12) +
  labs(subtitle="4: All features") )
ggsave("../fig/Nationscape/R2_RF_BIDEN2020_VIP_spec4_onlySubtitle.pdf")

ggpubr::ggarrange(v1,v2,v3,v4,
                  ncol=2,nrow=2) %>% annotate_figure(
                    top = text_grob("RF-based variable importance. Outcome: Biden vote intent in 2020",
                                    face = "italic", size = 14),
                    bottom = text_grob("Nationscape data: April - June 2020. N = 59,932"))
ggsave("../fig/Nationscape/NS_R2_all4_VIP.pdf")


# Comparison across specifications
rf_performance <- data.frame(spec = c("Gender, Race, Education, Income, Age (Set 1)",
                                      "Set 1 + Party ID (Set 2)",
                                      "Set 2 + Issues (Set 3)",
                                      "Set 3 + remaining information about respondents"),
                             RF = c(
                               ranger_wImp_spec1$finalModel$prediction.error,
                               ranger_wImp_spec2$finalModel$prediction.error,
                               ranger_wImp_spec3$finalModel$prediction.error,
                               ranger_wImp_spec4$finalModel$prediction.error)
                              )

rf_performance %>% 
  arrange(RF) %>%    
  mutate(spec=factor(spec, levels=spec)) %>%
  ggplot(aes(spec,RF)) +
  geom_segment( aes(xend=spec, yend=0)) +
  geom_point(size=2,color="navy") +
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Feature Set",y="Brier score",
       title = "Outcome: Biden vote intent\nComparison of OOB prediction errors (RF) across feature sets")
ggsave("../fig/Nationscape/Biden_RF_performance_FULL.pdf")

ggdotchart(rf_performance, x = "spec", y = "RF") + coord_flip() +
  labs(x = "",y="Brier score",
       title = "Comparison of OOB prediction errors (RF)\nacross feature sets")
ggsave("../fig/Nationscape/RF_performance_FULL_spec4_bidenv2.pdf")


###################
# HOLDOUT SET [OOS]
###################
Xm_Bidentest <- Xm[-biden_trainsetIndex,]

# Spec0 = ranger_wImp_spec0,
#Spec2 = ranger_wImp_spec2,
#Spec3 = ranger_wImp_spec3),
#Spec4 = ranger_wImp_spec4),

S0 <- extractPrediction(models = list(ranger_wImp_spec0),verbose=T,testX = Xm_Bidentest,testY = Xm_Bidentest$biden20)
S1 <- extractPrediction(models = list(ranger_wImp_spec1),verbose=T,testX = Xm_Bidentest,testY = Xm_Bidentest$biden20)
S2 <- extractPrediction(models = list(ranger_wImp_spec2),verbose=T,testX = Xm_Bidentest,testY = Xm_Bidentest$biden20)
S3 <- extractPrediction(models = list(ranger_wImp_spec3),verbose=T,testX = Xm_Bidentest,testY = Xm_Bidentest$biden20)
S4 <- extractPrediction(models = list(ranger_wImp_spec4),verbose=T,testX = Xm_Bidentest,testY = Xm_Bidentest$biden20)

S0_prep <- S0 %>% filter(dataType=="Test") %>% select(pred,obs)
S1_prep <- S1 %>% filter(dataType=="Test") %>% select(pred,obs)
S2_prep <- S2 %>% filter(dataType=="Test") %>% select(pred,obs)
S3_prep <- S3 %>% filter(dataType=="Test") %>% select(pred,obs)
S4_prep <- S4 %>% filter(dataType=="Test") %>% select(pred,obs)

confusionMatrix(S0_prep$pred,S3_prep$obs,positive="Biden")
confusionMatrix(S1_prep$pred,S3_prep$obs,positive="Biden")
confusionMatrix(S2_prep$pred,S3_prep$obs,positive="Biden")
confusionMatrix(S3_prep$pred,S3_prep$obs,positive="Biden")
confusionMatrix(S4_prep$pred,S3_prep$obs,positive="Biden")

L4 <- ifelse(predict(logit_spec4,newdata = Xm_Bidentest) >=.5,"Biden","Trump")
CM_L4 <- confusionMatrix(factor(L4),factor(Xm_Bidentest$biden20))


####################
# COMPARE SPECIFICATIONS
####################

pred_RF_holdoutsetBIDEN0 <- predict(ranger_wImp_spec0$finalModel, data=Xm_Bidentest)
pred_RF_holdoutsetBIDEN1 <- predict(ranger_wImp_spec1$finalModel, data=Xm_Bidentest)
pred_RF_holdoutsetBIDEN2 <- predict(ranger_wImp_spec2$finalModel, data=Xm_Bidentest)
pred_RF_holdoutsetBIDEN3 <- predict(ranger_wImp_spec3$finalModel, data=Xm_Bidentest)
pred_RF_holdoutsetBIDEN4 <- predict(ranger_wImp_spec4$finalModel, data=Xm_Bidentest)

 
RF_predictions_R2_v2 <- data.frame(actual_vote=Xm_Bidentest$biden20,
                                            spec1_trump = pred_RF_holdoutsetBIDEN1$predictions[,2],
                                            spec2_trump = pred_RF_holdoutsetBIDEN2$predictions[,2],          
                                            spec3_trump = pred_RF_holdoutsetBIDEN3$predictions[,2])
                                            spec4_biden = pred_RF_holdoutsetBIDEN4$predictions[,2])
write_csv(RF_predictions_R2_v2,"../output/Nationscape/RF_R2_predictions_v2.csv")
RF_predictions_R2_v2 <- read_csv("../output/Nationscape/xx.csv")

CM_spec1 <- confusionMatrix(factor(RF_predictions_R2_v2$spec1_trump>=.5),factor(RF_predictions_R2_v2$actual_vote=="Biden"))
CM_spec2 <- confusionMatrix(factor(RF_predictions_R2_v2$spec2_trump>=.5),factor(RF_predictions_R2_v2$actual_vote=="Biden"))
CM_spec3 <- confusionMatrix(factor(RF_predictions_R2_v2$spec3_trump>=.5),factor(RF_predictions_R2_v2$actual_vote=="Biden"))
CM_spec4 <- confusionMatrix(factor(RF_predictions_R2_v2$spec4_trump>=.5),factor(RF_predictions_R2_v2$actual_vote=="Biden"))

main_results_NS <- data.frame(spec = 
                                c("Gender, Race, Education,\nIncome, Age\n(Set 1)",
                                  "Set 1 + Party ID\n(Set 2)",
                                  "Set 2 + Issues\n(Set 3)",
                                  "Set 3\n + remaining information\nabout respondents"),
                              accuracy =c(
                                CM_spec1$overall[1],
                                CM_spec2$overall[1],
                                CM_spec3$overall[1],
                                CM_spec4$overall[1]),
                              lower = c(
                                CM_spec1$overall[3],
                                CM_spec2$overall[3],
                                CM_spec3$overall[3],
                                CM_spec4$overall[3]),
                              upper = c(
                                CM_spec1$overall[4],
                                CM_spec2$overall[4],
                                CM_spec3$overall[4],
                                CM_spec4$overall[4]))


main_results_NS %>%
  arrange(-accuracy) %>%    
  mutate(spec=factor(spec, levels=spec)) %>%
  ggplot(aes(x = accuracy, y = spec)) +
  geom_segment(aes(x = lower, xend = upper, y = spec, yend = spec),
               size = 2, color = "grey55") +
  geom_point(size = 1.25, shape = 21, stroke = 0.4, color = "black",
             fill = "black", shape = 21) +
  coord_cartesian(xlim = c(.6,.95)) +
  labs(x = "Accuracy (with 95% CI)", y = "Model features",
       title="Prediction accuracy (Nationscape holdout sample)") +
  #  scale_y_discrete(breaks = c()) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
ggsave("../fig/Nationscape/RF_R2_accuracy_V2.pdf")


# Summarize RFs
#pred_RF_holdoutset <- predict(mod5_spec4_FULL$RF$finalModel, data=Xm[-Xm_trainset_indices,])
# This is equivalent to the line above

# pred_RF_df <- cbind(pred_RF_holdoutset$predictions, actual_vote = Xm[-Xm_trainset_indices,"trump16"]) 
# pred_RF_df$predicted_vote <- ifelse(pred_RF_holdoutset$predictions[,2] >= pred_RF_holdoutset$predictions[,1],"Trump","Clinton") 

Xm_Bidentest$predicted_vote3 <- factor(ifelse(pred_RF_holdoutsetBIDEN3$predictions[,2] >= pred_RF_holdoutsetBIDEN3$predictions[,1],"Biden","Trump"))
Xm_Bidentest$`Prob(Biden3)` <- pred_RF_holdoutsetBIDEN3$predictions[,2]
Xm_Bidentest$`Prob(Trump3)` <- pred_RF_holdoutsetBIDEN3$predictions[,1]

Xm_Bidentest %>% mutate(biden_quintile = cut_number(`Prob(Biden3)` ,5,
                                   labels = c("Least likely to support Biden",
                                              "2nd",
                                              "Middle quintile",
                                              "4th",
                                              "Most likely to support Biden"))) %>%
  count(Men,biden_quintile) 

Xm_Bidentest %>% mutate(biden_narrow = ifelse(`Prob(Biden3)` <=.6 & `Prob(Biden3)` >=.4,"Prob.Biden 40%-60%","All other")) %>%
  count(biden_narrow,predicted_vote3,biden20) %>% arrange(desc(biden_narrow))

Xm_Bidentest %>% mutate(biden_deciles = cut_number(`Prob(Biden3)` ,10)) %>%
  count(Men,biden_deciles)
Xm_Bidentest %>% mutate(biden_10groups = cut_interval(`Prob(Biden3)` ,10)) %>%
  count(biden_10groups) %>% 
  ggplot(aes(x=biden_10groups,y=n,group=1)) + geom_line() + geom_point() +
  theme_minimal() +
  labs(x="Prob(Biden 2020) interval", y = "Number of respondents",subtitle = "Holdout set predicted probabilities")

confusionMatrix(factor(Xm_Bidentest$predicted_vote3),factor(Xm_Bidentest$biden20),
                positive="Biden")

# Main confusion matrix plot:
Xm_Bidentest %>% 
  select(predicted_vote3,biden20) %>% 
  table() %>%
  as_tibble() %>% 
  rename(N=n) -> cm_toplot

plot_confusion_matrix(cm_toplot,
                      targets_col = "biden20", 
                      predictions_col = "predicted_vote3",
                      palette = "Purples")
                      add_col_percentages = F, font_counts = font(size=6)
#font_normalized = (font(size=6)),

ggsave("../fig/Nationscape/Biden_Conf_Matrix_RF_spec4.pdf")




par(mfrow=c(1,3))
plot(pred_RF_holdoutset3$predictions[,1],
     pred_RF_holdoutset$predictions[,1],
     col = rgb(red = .1, green = .1, blue = .1, alpha = 0.07), 
     pch=16,
     xlab="Specification 3",ylab="Specification 4")
plot(pred_RF_holdoutset2$predictions[,1],
     pred_RF_holdoutset$predictions[,1],
     col = rgb(red = .1, green = .1, blue = .1, alpha = 0.07), 
     pch=16,
     xlab="Specification 2",ylab="Specification 4")
plot(pred_RF_holdoutset1$predictions[,1],
     pred_RF_holdoutset$predictions[,1],
     col = rgb(red = .1, green = .1, blue = .1, alpha = 0.07), 
     pch=16,
     xlab="Specification 1",ylab="Specification 4")
dev.copy(pdf,"../fig/Nationscape/RF_3correlations_holdoutset.pdf")
dev.off()

par(mfrow=c(1,1))




# JZ TO DO
Xm_Bidentest %>%
  mutate(age_quintile = cut_number(age,5)) %>%
  ggplot(aes(x=`Prob(Biden)_M0`,fill=biden20)) + geom_histogram() +
  facet_wrap(~age_quintile) +
  labs(y="Number of out-of-sample observations",
       x = "Prob(Biden)", fill = "Actual vote\nintent") +
  scale_fill_manual(values = c("red","steelblue"))

# Here's how to use a labeller
Xm_Bidentest %>%
  mutate(age_quintile = cut_number(age,5)) %>%
  ggplot(aes(x=`Prob(Biden)_M0`,fill=biden20)) + geom_histogram() +
  facet_wrap(~educ_category_4,
             labeller = labeller(educ_category_4 = c("0" = "Less than college graduates",
                                                     "1" = "College graduates"))) +
  labs(y="Number of out-of-sample observations",
       x = "Prob(Biden)", fill = "Actual vote\nintent") +
  scale_fill_manual(values = c("red","steelblue"))


# Make some nice histograms
# names(pred_RF_df) <- c("P(Clinton)" , "P(Trump)", "Actual Vote","Predicted vote")
# levels(pred_RF_df$`Actual Vote`) <- c("Clinton voters", "Trump voters")
# pred_RF_df %>% ggplot(aes (x= `P(Trump)`)) + geom_histogram() + facet_wrap(~`Actual Vote`) +
#   labs(x = "Predicted probability of the Trump vote",
#        y= "Number of respondents (in the holdout sample)")
# dev.copy(pdf,"ranger_FULL.pdf")
# dev.off()

(Xm_Bidentest %>%
    filter(Men==1) %>%
    mutate(wnc =  case_when(white_noncollege == 0 ~ "White college educated men",
                            white_noncollege == 1 ~ "White non-college men")) %>%
    mutate(jwa =  case_when(Jwall_agree == 0 ~ "Oppose building the wall",
                            Jwall_agree == 1 ~ "Support building the wall")) %>%
    mutate(mm =  case_when(Men == 0 ~ "Female voters",
                           Men == 1 ~ "Male voters")) %>%
    mutate(pid =  case_when((pid7_1 ==1 | pid7_2 ==1 | pid7_3 == 1) ~ "Democrats",
                            pid7_4 == 1 ~ "Independents",
                            (pid7_5 ==1 | pid7_6 ==1 | pid7_7 == 1) ~ "Republicans")) %>%
    ggplot(aes (x= `Prob(Biden)`)) + geom_histogram(bins=100,
                                                    aes(y = ..density..) ) +
    labs(y="Percent",
         x="Random-forest-based probability of supporting Biden",
         subtitle="Predicted probabilities for 16,264 respondents in the holdout set (Nationscape)") +
    facet_grid(~ pid) -> pidplot)
gg_check(pidplot)
ggsave("../fig/Nationscape/Biden_RF_holdout_pid3.pdf")


Xm_Bidentest %>%
  mutate(`Education split` =  case_when(educ_category_1 == 1 ~ "Low. ed. category",
                                        educ_category_4==1 ~ "College graduates")) %>%
  filter(!is.na(`Education split`)) %>%
  ggdensity(x = "Prob(Biden)",
            color = "Education split",fill = "Education split") +
  ggtitle("RF-based predictions on the Nationscape holdout set")
ggsave("../fig/Nationscape/Biden_NS_z_educ.pdf")

Xm_Bidentest %>%
  mutate(ideology =  case_when((ideo5_1==1 |  ideo5_2==1) ~ "Liberals",
                               ideo5_3==1 ~ "Moderates",
                               (ideo5_4==1 | ideo5_5==1) ~ "Conservatives")) %>%
  filter(!is.na(ideology)) %>%
  ggviolin(x = "ideology", 
           y = "Prob(Biden)", fill = "ideology",
           palette = c( "#FC4E07", "#E7B800", "#00AFBB")) +
  labs(fill = "")
ggsave("../fig/Nationscape/Biden_NS_ideology_PrTrump.pdf")





# Hexbin stuff

library(RColorBrewer)
my_colors <- colorRampPalette(brewer.pal(8,'Greens'))

hexbinplot(pred_RF_holdoutset$predictions[,2] ~ pred_RF_holdoutset3$predictions[,2],
           xlab="Predicted probabilities\nfrom Specification 4",
           ylab="Predicted probabilities\nfrom Specification 3",
           colramp=my_colors,
           colorcut=c(0,.005,.015,.05,.1,.2,1),
           xbins=10,
           ybins=10,
           border = gray(.85),
           main="Correlation between the predicted probability of Trump vote\nin Specification 3 vs Specification 4")
dev.copy(pdf,"hex_corr_spec4vs3.pdf")
dev.off()

####################
# CONFUSION MATRICES [by group]
####################


Xm_testset_data %>% filter(pid7_7==1) %>%
  select(predicted_vote,trump16) %>% table() %>%
  as_tibble() %>% 
  rename(N=n) -> PID7m
plot_confusion_matrix(PID7m,
                      targets_col = "trump16", 
                      predictions_col = "predicted_vote",
                      palette = "Reds",
                      add_col_percentages = F,
                      font_normalized = (font(size=6)),
                      font_counts = font(size=5)) + 
  ggplot2::labs(title="Strong Republicans",
                x="Actual vote")
ggsave("../fig/Nationscape/RF_CM_strong_Republicans.png")


Xm_testset_data %>% filter(pid7_5==1 | pid7_6==1 | pid7_7==1) %>%
  select(predicted_vote,trump16) %>% table() %>%
  as_tibble() %>% 
  rename(N=n) -> repub_cm
plot_confusion_matrix(repub_cm,
                      targets_col = "trump16", 
                      predictions_col = "predicted_vote",
                      palette = "Reds",
                      add_col_percentages = F,
                      add_row_percentages = F,
                      font_normalized = (font(size=6)),
                      font_counts = (font(size=4))) +
  ggplot2::labs(title="Republicans (incl. leaners)",
                x="Actual vote")-> REPS


Xm_testset_data %>% filter(pid7_4==1) %>%
  select(predicted_vote,trump16) %>% table() %>%
  as_tibble() %>% 
  rename(N=n) -> ind_cm
plot_confusion_matrix(ind_cm,
                      targets_col = "trump16", 
                      predictions_col = "predicted_vote",
                      palette = "Greens",
                      add_col_percentages = F,
                      add_row_percentages = F,
                      font_normalized = (font(size=6)),
                      font_counts = (font(size=4))) + 
  labs(title="Independents",x="Actual vote") -> INDS

Xm_testset_data %>% filter(pid7_1==1 | pid7_2==1 | pid7_3==1) %>%
  select(predicted_vote,trump16) %>% table() %>%
  as_tibble() %>% 
  rename(N=n) -> dem_cm
plot_confusion_matrix(dem_cm,
                      targets_col = "trump16", 
                      predictions_col = "predicted_vote",
                      palette = "Blues",
                      add_col_percentages = F,
                      add_row_percentages = F,
                      font_normalized = (font(size=6)),
                      font_counts = (font(size=4))) +
  ggplot2::labs(title="Democrats (incl. leaners)",x="Actual vote") -> DEMS


ggarrange(DEMS,INDS,REPS,ncol=3)
ggsave("../fig/Nationscape/RF_CM_byPID3.png")
