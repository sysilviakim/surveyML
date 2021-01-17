######################################
# ENSEMBLE METHODS
# ESTIMATE several algorithms simultanously
######################################


source("R/NS_0_labels.R")
source("R/NS_1_data.R")
# GOOD TO RUN THIS for "seed <-" 
# even though trainControl() is defined in the current file
source("R/NS_2_prep_ML.R")

library(assertthat)
library(hrbrthemes) # useful for gg_check()
library(mlbench)
library(nnet)

# library(doParallel)
# cl <- makePSOCKcluster(4)
# registerDoParallel(cl) # All subsequent models are then run in parallel

## The key here, of course, is to actually save importance
# from measures from ranger
mod5_spec4_FULL <- caretList(
  trump16 ~ .,
  data = Xm[Xm_trainset_indices, ],
  tuneList = list(
    Logit = caretModelSpec(method = "glm", family = binomial("logit")),
    Tree = caretModelSpec(method = "rpart", tuneLength = 10),
    RF = caretModelSpec(
      method = "ranger", importance = "permutation", tuneLength = 10
    ),
    # KRLS = caretModelSpec(
    #   method = "krlsPoly", trace = FALSE, tuneLength = 10
    # ),
    # won't work classification [which is strange]
    LASSO = caretModelSpec(
      method = "glmnet", tuneLength = 10,
      preProcess = c("center", "scale")
    )
  ),
  trControl = trainControl(
    method = "cv",
    number = 10,
    seeds = rep_seeds(),
    savePredictions = "final",
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    index = createFolds(Xm$trump16[Xm_trainset_indices], 10)
    # , allowParallel = TRUE [for reproductibility purposes,
    # let's not run things in parallel anymore]
  )
)

mod5_spec1_FULL <- caretList(
  trump16 ~ educ_category_1 +
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
  data = Xm[Xm_trainset_indices, ],
  tuneList = list(
    Logit = caretModelSpec(method = "glm", family = binomial("logit")),
    Tree = caretModelSpec(method = "rpart", tuneLength = 10),
    RF = caretModelSpec(
      method = "ranger", importance = "permutation", tuneLength = 10
    ),
    NeuralNet = caretModelSpec(method = "nnet", trace = FALSE, tuneLength = 10),
    LASSO = caretModelSpec(
      method = "glmnet", tuneLength = 10,
      preProcess = c("center", "scale")
    )
  ),
  trControl = trainControl(
    method = "cv",
    number = 10,
    seeds = rep_seeds(),
    savePredictions = "final",
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    index = createFolds(Xm$trump16[Xm_trainset_indices], 10)
  )
)

mod5_spec2_FULL <- caretList(
  trump16 ~ educ_category_1 +
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
  data = Xm[Xm_trainset_indices, ],
  tuneList = list(
    Logit = caretModelSpec(method = "glm", family = binomial("logit")),
    Tree = caretModelSpec(method = "rpart", tuneLength = 10),
    RF = caretModelSpec(
      method = "ranger", importance = "permutation", tuneLength = 10
    ),
    NeuralNet = caretModelSpec(
      method = "nnet", trace = FALSE, tuneLength = 10
    ),
    LASSO = caretModelSpec(
      method = "glmnet", tuneLength = 10,
      preProcess = c("center", "scale")
    )
  ),
  trControl = trainControl(
    method = "cv",
    number = 10,
    seeds = rep_seeds(),
    savePredictions = "final", # This is useful
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    index = createFolds(Xm$trump16[Xm_trainset_indices], 10)
  )
)

assert_that(names(Xm)[39] == "Jabortion_latepermit_DISAGREE"
& names(Xm)[87] == "inc_group_5")
mod5_spec3_FULL <- caretList(
  trump16 ~ .,
  data = Xm[Xm_trainset_indices, c(1, 39:87)],
  tuneList = list(
    Logit = caretModelSpec(method = "glm", family = binomial("logit")),
    Tree = caretModelSpec(method = "rpart", tuneLength = 10),
    RF = caretModelSpec(
      method = "ranger", importance = "permutation", tuneLength = 10
    ),
    NeuralNet = caretModelSpec(
      method = "nnet", trace = FALSE, tuneLength = 10
    ),
    LASSO = caretModelSpec(
      method = "glmnet", tuneLength = 10,
      preProcess = c("center", "scale")
    )
  ),
  trControl = trainControl(
    method = "cv",
    number = 10,
    seeds = rep_seeds(),
    savePredictions = "final", # This is useful
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    index = createFolds(Xm$trump16[Xm_trainset_indices], 10)
  )
)


# stopCluster(cl)
# stopImplicitCluster()

saveRDS(
  mod5_spec1_FULL,
  "output/Nationscape/spec1_nationscape_FULLtestsample_Aug4.RDS"
)
saveRDS(
  mod5_spec2_FULL,
  "output/Nationscape/spec2_nationscape_FULLtestsample_Aug4.RDS"
)
saveRDS(
  mod5_spec3_FULL,
  "output/Nationscape/spec3_nationscape_FULLtestsample_Aug4.RDS"
)
saveRDS(
  mod5_spec4_FULL,
  "output/Nationscape/spec4_nationscape_FULLtestsample_Aug4.RDS"
)
