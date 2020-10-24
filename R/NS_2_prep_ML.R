seed <- 10012

rep_seeds <- function(folds = 10, n = 1e4, seed = 123) {
  set.seed(seed)
  ## (n_repeats * nresampling) + 1
  seeds <- vector(mode = "list", length = folds + 1)
  for (i in seq(folds)) {
    seeds[[i]] <- sample.int(n = n, n)
  }
  seeds[[folds + 1]] <- sample.int(n = n, 1)
  return(seeds)
}
  
# Prepare training validation sets
set.seed(seed)

if (outcome == "trump16") {

Xm_trainset_indices <- createDataPartition(Xm$trump16,times = 1,
                                           p=0.8, list=F)  # Note: THESE ARE INDICES ONLY
Xm_testset_data <- Xm[-Xm_trainset_indices,]
}

if (outcome == "biden20") {
  
  biden_trainsetIndex <- createDataPartition(Xm$biden20, p = .8, 
                                            list = F, 
                                            times = 1)
}

# Use rep_seeds() as suggested by Silvia on 7/25/20:
fit_control_CV <- trainControl(
  method = "cv",
  number = 10,
  ## Provides ROC summary stats
  summaryFunction = caret::twoClassSummary,
  allowParallel = TRUE,
  verboseIter = FALSE,
  seeds = rep_seeds(),
  classProbs = TRUE,
  savePredictions = "final"
)

fit_control_repCV <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  ## Provides ROC summary stats
  summaryFunction = caret::twoClassSummary,
  allowParallel = TRUE,
  verboseIter = FALSE,
  seeds = rep_seeds(),
  classProbs = TRUE,
  savePredictions = "final"
)

# fit_control_basic <- trainControl(
#   method = "cv",
#   number = 10,
#   savePredictions = "final", # This is useful
#   classProbs = TRUE
# )

# fit_control <- trainControl(
#   method = "repeatedcv",
#   number = 10, # number of folds
#   repeats = 10, # number of repeats
#   savePredictions = "final", # This is useful
#   classProbs = TRUE,
#   summaryFunction = twoClassSummary
# )