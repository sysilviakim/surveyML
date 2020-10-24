# Read in existing the models:
mod5_spec1_FULL <- readRDS("../output/Nationscape/spec1_nationscape_FULLtestsample_Aug4.RDS")
mod5_spec2_FULL <- readRDS("../output/Nationscape/spec2_nationscape_FULLtestsample_Aug4.RDS")
mod5_spec3_FULL <- readRDS("../output/Nationscape/spec3_nationscape_FULLtestsample_Aug4.RDS")
mod5_spec4_FULL <- readRDS("../output/Nationscape/spec4_nationscape_FULLtestsample_Aug4.RDS")


#names(all.models) <- sapply(all.models, function(x) x$method)
ens_MIN_ROC <- sapply(mod5_spec4_FULL, function(x) min(x$results$ROC))
ens_MEAN_ROC <- sapply(mod5_spec4_FULL, function(x) mean(x$results$ROC))
ens_MAX_ROC <- sapply(mod5_spec4_FULL, function(x) max(x$results$ROC))

# PREPARE ROC COMPARISON:
ENS_ROC <- data.frame(models = c("Logit",
                                 "CART",
                                 "Random Forest",
                                 "LASSO"),
                      min_ROC = ens_MIN_ROC,
                      mean_ROC = ens_MEAN_ROC,
                      max_ROC = ens_MAX_ROC)

ENS_ROC %>%
  mutate(models = factor(models, levels=c("CART",
                                          "LASSO",
                                          "Logit",
                                          "Random Forest"))) %>%
  arrange(mean_ROC) %>%
  ggplot(aes(x = mean_ROC, y = models)) +
  geom_segment(aes(x = min_ROC, xend = max_ROC, y = models, yend = models),
               size = 2, color = "grey85") +
  geom_point(size = 1.25, shape = 21, stroke = 0.4, color = "black",
             fill = "black", shape = 21) +
  coord_cartesian(xlim = c(.6,1)) +
  labs(x = "ROC range", y = "Model",
       title = "ROC min to max range", subtitle = "The dot in each row shows the mean ROC") +
  #  scale_y_discrete(breaks = c()) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
ggsave("../fig/Nationscape/Ensemble_ROC_comparisonFULLtestset.pdf")


mod5_spec4_FULL$Tree$results %>%
  mutate(L = ROC - 1.96*ROCSD, 
         U = ROC + 1.96*ROCSD) %>%
  filter(cp <=.4) %>%
  ggplot(aes(x=cp,y=ROC)) + 
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin=L, ymax=U)) +
  labs(x = "Complexity parameter")
ggsave("../fig/Nationscape/CART_tuning_FULL.pdf")

mod5_spec4_FULL$RF$results %>%
  filter(splitrule=="gini") %>%
  mutate(L = ROC - 1.96*ROCSD, 
         U = ROC + 1.96*ROCSD) %>%
  ggplot(aes(x=mtry,y=ROC)) + 
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin=L, ymax=U)) +
  labs(x = "Number of features available at each split",
       title = "Tuning the Random Forest algorithm")
ggsave("../fig/Nationscape/RF_tuning_FULL.pdf")

mod5_spec4_FULL$RF$results %>%
  filter(splitrule=="gini") %>%
  mutate(L = Sens - 1.96*SensSD, 
         U = Sens + 1.96*SensSD) %>%
  ggplot(aes(x=mtry,y=Sens)) + 
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin=L, ymax=U)) +
  ylim(.91,.94) +
  labs(x = "Number of features available at each split",
       title = "Tuning the Random Forest algorithm",
       y = "Sensitivity")
ggsave("../fig/Nationscape/RF_tuning_FULL.pdf")

#scale_x_continuous(breaks = 0:1) +

###################
# MISC information
###################

# What are the correlations across models? 
modelCor(resamples(mod5_spec4_FULL))

modelCor(resamples(mod5_spec4_FULL)) %>% corrplot::corrplot(method="number",type="upper",cl.ratio = .4, cl.align = "c")
dev.copy(pdf,"../fig/Nationscape/alg_correlations.pdf")
dev.off()
                   
# Inspect how models were tuned; mtry & min.node.size make very little difference for RF
mod5_spec4_FULL$RF$results
# alpha and lambda matter somewhat
mod5_spec4_FULL$LASSO$results %>% head()
# NN: Decay and size make a lot of difference
mod5_spec4_FULL$NeuralNet$results %>% head()
# CARTY: cp makes a lot of difference, obviously
mod5_spec4_FULL$Tree$results
# NO TUNING!
mod5_spec4_FULL$Logit$results %>% head()

### Code below "in progress" 8/5/2020

results <- resamples(mod5_spec4_FULL)


summary(results)
dotplot(results,
        main = "Performance on Nationscape data (Y = Trump vote in 2016)",
        metric = "ROC")
dev.copy(pdf,"spec5_ROC_NS.pdf")
dev.off()

# This is OK, but BETTER TO select() exactly the models you want...
histogram(~mod5_spec4_FULL$RF$pred$Trump|mod5_spec4_FULL$RF$pred$obs)

# MAKE LIN. combinations of models
linear_ensemble <- caretStack(mod5_spec4_FULL, method='glm', trControl=trainControl(method='cv'))
linear_ensemble$error


greedy_ensemble <- caretEnsemble(mod5_spec4_FULL,
                                 trControl=trainControl(
                                   number=2
                                 ))

summary(greedy_ensemble)

