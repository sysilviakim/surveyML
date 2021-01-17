atest <- a %>%
  slice_sample(prop = .05) %>%
  select(
    trump_v_biden, Jwall_agree,
    Jdeportation_agree,
    white_noncollege,
    Men, Black, Asian,
    Hispanic,
    age,
    pid7,
    ideo5,
    educ_category,
    inc_group,
    household_gun_owner,
    in_union,
    is_evangelical
  ) %>%
  filter(complete.cases(.))

# atest$trump_biden <- factor(atest$trump_biden,levels=c(1,2,999))

rtest <- train(trump_biden ~ .,
  data = atest,
  method = "ranger",
  importance = "permutation",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    classProbs = FALSE,
    savePredictions = "final"
  )
)

# library(pre)
library(xrf)
set.seed(10012)
ax <- zap_labels(atest)
# a2 <- as.data.frame(atest)

rules <- pre(trump_v_biden ~ ., data = ax)

summary(rules)
coef(rules, penalty.par.val = "lambda.1se")[1:15, ]
nterms

plot(rules, nterms = 10)

pre::importance(rules)
pre:interact(rules, c("pid7", "Jwall_agree"))

pre::pairplot(
  rules,
  c("pid7", "Men")
)
# Type of plot to be generated.
# type = "heatmap" yields a heatmap plot,
# type = "contour" yields a contour plot,
# type = "both" yields a heatmap plot with added contours,
# type = "perspective" yields a three di- mensional plot.

rules_xrf <- pre(factor(trump_v_biden) ~ ., data = ax, family = "binomial")
