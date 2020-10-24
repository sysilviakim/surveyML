library(tidyverse)
library(ggpubr) # ggdensity and ggarrange; https://rpkgs.datanovia.com/ggpubr/index.html
library(cvms) # use for plot_confusion_matrix()
library(hrbrthemes) # useful for gg_check()
library(hexbin)

source("NS_0_labels.R")
source("NS_1_data.R")
source("NS_2_prep_ML.R")

# Read in the models:
mod5_spec1_FULL <- readRDS("../output/Nationscape/spec1_nationscape_FULLtestsample_Aug4.RDS")
mod5_spec2_FULL <- readRDS("../output/Nationscape/spec2_nationscape_FULLtestsample_Aug4.RDS")
mod5_spec3_FULL <- readRDS("../output/Nationscape/spec3_nationscape_FULLtestsample_Aug4.RDS")
mod5_spec4_FULL <- readRDS("../output/Nationscape/spec4_nationscape_FULLtestsample_Aug4.RDS")

# Comparison across specifications
rf_performance <- data.frame(spec = c("Gender, Race, Education, Income, Age (Set 1)",
                                      "Set 1 + Party ID (Set 2)",
                                      "Set 2 + Issues (Set 3)",
                                      "Set 3 + remaining information about respondents"),
                             RF = c(
                               mod5_spec1_FULL$RF$finalModel$prediction.error,
                               mod5_spec2_FULL$RF$finalModel$prediction.error,
                               mod5_spec3_FULL$RF$finalModel$prediction.error,
                               mod5_spec4_FULL$RF$finalModel$prediction.error)
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
       title = "Comparison of OOB prediction errors (RF)\nacross feature sets")
ggsave("../fig/Nationscape/RF_performance_FULLv1.pdf")

ggdotchart(rf_performance, x = "spec", y = "RF") + coord_flip() +
  labs(x = "",y="Brier score",
       title = "Comparison of OOB prediction errors (RF)\nacross feature sets")
ggsave("../fig/Nationscape/RF_performance_FULLv2.pdf")



names(mod5_spec4_FULL$RF$finalModel$variable.importance) <- relaber(names(mod5_spec4_FULL$RF$finalModel$variable.importance))

vip(mod5_spec4_FULL$RF$finalModel,
    num_features = 10) + labs(title="RF-based variable importance (Nationscape)")
ggsave("../fig/Nationscape/RF_VIP_Aug4_FULLmodel_n10.pdf")
 
###################
# HOLDOUT SET [OOS]
###################

# Summarize RFs
# First focus on SPEC4:

#pred_RF_holdoutset <- predict(mod5_spec4_FULL$RF$finalModel, data=Xm[-Xm_trainset_indices,])
# This is equivalent to the line above
pred_RF_holdoutset <- predict(mod5_spec4_FULL$RF$finalModel, data=Xm_testset_data)

# pred_RF_df <- cbind(pred_RF_holdoutset$predictions, actual_vote = Xm[-Xm_trainset_indices,"trump16"]) 
# pred_RF_df$predicted_vote <- ifelse(pred_RF_holdoutset$predictions[,2] >= pred_RF_holdoutset$predictions[,1],"Trump","Clinton") 

Xm_testset_data$predicted_vote <- factor(ifelse(pred_RF_holdoutset$predictions[,2] >= pred_RF_holdoutset$predictions[,1],"Trump","Clinton"))
Xm_testset_data$`Prob(Trump)` <- pred_RF_holdoutset$predictions[,2]
Xm_testset_data$`Prob(Clinton)` <- pred_RF_holdoutset$predictions[,1]

# Rich set of statistics
confusionMatrix(factor(Xm_testset_data$predicted_vote),factor(Xm_testset_data$trump16),
                positive="Trump")

# Main confution matrix plot:
Xm_testset_data %>% 
  select(predicted_vote,trump16) %>% 
  table() %>%
  as_tibble() %>% 
  rename(N=n) -> cm_toplot

plot_confusion_matrix(cm_toplot,
                      targets_col = "trump16", 
                      predictions_col = "predicted_vote",
                      palette = "Purples",
                      add_col_percentages = F,
                      font_normalized = (font(size=6)),
                      font_counts = font(size=5)) 
ggsave("../fig/Nationscape/Conf_Matrix_RF.pdf")


# JZ TO DO
# age_names <- c("Bottom quintile", "2nd quintile", "Middle quintile", "4th quintile", "Top quintile")
# Xm_testset_data %>%
# mutate(age_quintile = cut_number(age,5))
# facet_grid(.~age_quintile, labeller = as_labeller(age_names))

# Make some nice histograms
# names(pred_RF_df) <- c("P(Clinton)" , "P(Trump)", "Actual Vote","Predicted vote")
# levels(pred_RF_df$`Actual Vote`) <- c("Clinton voters", "Trump voters")
# pred_RF_df %>% ggplot(aes (x= `P(Trump)`)) + geom_histogram() + facet_wrap(~`Actual Vote`) +
#   labs(x = "Predicted probability of the Trump vote",
#        y= "Number of respondents (in the holdout sample)")
# dev.copy(pdf,"ranger_FULL.pdf")
# dev.off()

(Xm_testset_data %>%
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
  ggplot(aes (x= `Prob(Trump)`)) + geom_histogram(bins=100,
                                                  aes(y = ..density..) ) +
  labs(y="Percent",
       x="Random-forest-based probability of voting for Trump",
       subtitle="Predicted probabilities for 16,264 respondents in the holdout set (Nationscape)") +
  facet_grid(~ pid) -> pidplot)
gg_check(pidplot)
ggsave("../fig/Nationscape/RF_holdout_pid3.pdf")


Xm_testset_data %>%
mutate(ideology =  case_when((ideo5_1==1 |  ideo5_2==1) ~ "Liberals",
                        ideo5_3==1 ~ "Moderates",
                        (ideo5_4==1 | ideo5_5==1) ~ "Conservatives")) %>%
ggdensity(x = "Prob(Clinton)",
          color = "ideology",fill = "ideology") +
  ggtitle("RF-based predictions on the Nationscape holdout set")
ggsave("../fig/Nationscape/Z_NS_ideology.pdf")

Xm_testset_data %>%
mutate(ideology =  case_when((ideo5_1==1 |  ideo5_2==1) ~ "Liberals",
                             ideo5_3==1 ~ "Moderates",
                             (ideo5_4==1 | ideo5_5==1) ~ "Conservatives")) %>%
  filter(!is.na(ideology)) %>%
  ggviolin(x = "ideology", 
         y = "Prob(Trump)", fill = "ideology",
         palette = c( "#FC4E07", "#E7B800", "#00AFBB")) +
  labs(fill = "")
ggsave("../fig/Nationscape/NS_ideology_PrTrump.pdf")



####################
# COMPARE SPECIFICATIONS
####################

pred_RF_holdoutset1 <- predict(mod5_spec1_FULL$RF$finalModel, data=Xm_testset_data)
pred_RF_holdoutset2 <- predict(mod5_spec2_FULL$RF$finalModel, data=Xm_testset_data)
pred_RF_holdoutset3 <- predict(mod5_spec3_FULL$RF$finalModel, data=Xm_testset_data)
# 
# RF_predictions_4specifications <- data.frame(actual_vote=Xm_testset_data$trump16,
#                                              spec4_trump = pred_RF_holdoutset$predictions[,2],
#                                              spec1_trump = pred_RF_holdoutset1$predictions[,2],
#                                              spec2_trump = pred_RF_holdoutset2$predictions[,2],
#                                              spec3_trump = pred_RF_holdoutset3$predictions[,2])
# write_csv(RF_predictions_4specifications,"../output/Nationscape/RF_predictions_4specifications_FULL_Aug5.csv")
RF_predictions_4specifications <- read_csv("../output/Nationscape/RF_predictions_4specifications_FULL_Aug5.csv")

CM_spec1 <- confusionMatrix(factor(RF_predictions_4specifications$spec1_trump>=.5),factor(Xm_testset_data$trump16=="Trump"))
CM_spec2 <- confusionMatrix(factor(RF_predictions_4specifications$spec2_trump>=.5),factor(Xm_testset_data$trump16=="Trump"))
CM_spec3 <- confusionMatrix(factor(RF_predictions_4specifications$spec3_trump>=.5),factor(Xm_testset_data$trump16=="Trump"))
CM_spec4 <- confusionMatrix(factor(RF_predictions_4specifications$spec4_trump>=.5),factor(Xm_testset_data$trump16=="Trump"))

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
ggsave("../fig/Nationscape/RF_accuracy_4_specification.pdf")

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
# CONFUSION MATRICES
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
