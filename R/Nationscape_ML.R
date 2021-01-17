# Nationscape_ML_models.R

library(tidyverse)
library(haven)
library(fastDummies)
library(rpart)
library(rpart.plot)
library(ranger)
require(fastDummies)
library(tuneRanger)
library(vip)
library(pdp)
library(randomForestExplainer)

a <- read_dta("~/Dropbox/B_polls/Nationscape/nationscape_CD_merged.dta")

relaber <- function(x) {
  x <- gsub("immig_full_theta","Immigration dim.",
            gsub("econ_cons_theta","Economic dim.",
                 gsub("racial10_C","Racial attitudes",
                      gsub("pid7","PID7",
                           gsub("Jwall_agree","Build the wall",
                           gsub("culture_full_theta","Cultural dim.",x))))))
  return(x)
}


# Choose your desired outcome -- Uncomment as appropriate:
outcome <- "trump16"
# outcome <- "trump_v_biden"

# Decide whether you want these sets of identity variables:
gender_vars <- c("gender_attitudes_complain","gender_attitudes_logical","gender_attitudes_maleboss")
racial_att_vars <- c("racial_attitudes_tryhard",
                     "racial_attitudes_generations",
                     "racial_attitudes_marry",
                     "racial_attitudes_date",
                     "discrimination_blacks",
                     "discrimination_muslims",
                     "discrimination_women",
                     "discrimination_men",
                     "discrimination_whites",
                     "discrimination_christians")

# Create the dataset used for training (all columns should be potential predictors):
a_dat_ranger <- a %>% select(outcome,
                             gender_vars,
                             racial_att_vars,
                             Jabortion_latepermit_DISAGREE,
                             Jabortion_never_agree,
                             Jabortion_mostlypermit_DISAGREE,
                             Jguns_bg_DISAGREE,
                             Jbanguns_DISAGREE,
                             Jban_AR_DISAGREE,
                             Jlimit_magazines_DISAGREE,
                             Jgov_ins_agree,
                             Jpublic_option_agree,
                             Jhealth_subsidies_agree,
                             Jraise_upper_tax_agree,
                             Jmctaxes_agree,
                             Jestate_NOT_repleal,
                             Jcollege_agree,
                             Jcap_agree,
                             Jdreamers_path_DISAGREE,
                             Jpath4all_DISAGREE,
                             Jwall_agree,
                             Jdeportation_agree,
                             white_noncollege,
                             Men,Black,
                             Hispanic,
                             age,
                             pid7,
                             ideo5,
                             education,
                             household_income,
                             household_gun_owner,
                             in_union,
                             is_evangelical,
                             language,
                             orientation_group,
                             sc_knowledge,
                             sen_knowledge,
                             news_sources_telemundo, news_sources_other, news_sources_npr, news_sources_new_york_times, news_sources_network, news_sources_msnbc, news_sources_localtv, news_sources_local_newspaper, news_sources_fox, news_sources_facebook, news_sources_cnn, news_sources_amtalk)

# One-hot encoding
# WARNING THIS SUBSETTING IS FRAGILE: what you really want is something like [,"pid7":"news_sources_amtalk"] 
a_dat_ranger[,39:61] <- lapply(a_dat_ranger[,39:61] , factor)
Xm1 <- dummy_cols(a_dat_ranger,remove_selected_columns = T)
Xm <- Xm1 %>% filter(complete.cases(.))


set.seed(10012)
assignment80_20 <- sample(1:2, size = nrow(Xm), prob = c(.8,.2), replace = TRUE)

a_trainset <- a[assignment80_20 == 1, ] 
a_testset <- a[assignment80_20 == 2, ]

#trainset <- sample()

# Basic tree

rp <- rpart(trump16 ~., 
            data=Xm[assignment80_20 == 1,],
            control=rpart.control(cp = 0.01))

# Relabel output
rp$frame$var <- relaber(rp$frame$var)
rpart.plot(rp,type=5,branch=.4,extra=1)
printcp(rp)


# RF
task <- makeRegrTask(data=Xm,target = outcome)
estimateTimeTuneRanger(task)

res = tuneRanger(task, measure = list(mse), num.trees = 250, 
                 num.threads = 2, iters = 50, save.file.path = NULL)

res_fast <- tuneMtryFast(trump16 ~ ., data = Xm)

ran_basic <- ranger(trump16 ~ ., data = Xm,importance = "permutation",
                    classification = F)

ran_tuned <- ranger(trump16 ~ ., data = Xm, 
                    mtry = 6, 
                    min.node.size = 2,
                    sample.fraction = 0.7326043,
                    importance = "permutation")

# Simple
p1 <- partial(ran_basic, pred.var = "age", plot = TRUE,
              plot.engine = "ggplot2")  + labs(y="Trump vote in 2016", x="Age")

# Variable importance plot
# (this gives us a ranking of the most important variables)
vip(ran_basic)



library(glmnet)
library(glmnetUtils)

# Lasso is ok, but factors are annoying
# names(A)
# A[,11:34] <- lapply(A[,11:34] , factor)
# D <- dummy_cols(A,remove_selected_columns = T)
# names(D)
# 
# lasso1_lm <- cva.glmnet(trump16 ~ ., data=D)
# 
# lasso1_logit <- glmnet(trump16 ~ ., data=D, family = "binomial")
