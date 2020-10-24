# Prep dataset for Biden analysis (i.e. use eligible voters only)

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

library(caret)
library(caretEnsemble)
library(mlbench)
library(pROC)

a <- read_dta("~/Dropbox/B_polls/Nationscape/nationscape_CD_merged.dta")

# Choose your desired outcome -- Uncomment as appropriate:
outcome <- "biden20"
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

group_attitudes <- c("asians_favor_withDK",
                     "blacks_favor_withDK",
                     "whites_favor_withDK",
                     "democrats_favor_withDK",
                     "labor_unions_favor_withDK",
                     "latinos_favor_withDK",
                     "lgbt_favor_withDK",
                     "muslims_favor_withDK",
                     "republicans_favor_withDK",
                     "socialists_favor_withDK",
                     "the_police_favor_withDK",
                     "undocumented_favor_withDK",
                     "asians_unfavor_withDK",
                     "blacks_unfavor_withDK",
                     "whites_unfavor_withDK",
                     "democrats_unfavor_withDK",
                     "labor_unions_unfavor_withDK",
                     "latinos_unfavor_withDK",
                     "lgbt_unfavor_withDK",
                     "muslims_unfavor_withDK",
                     "republicans_unfavor_withDK",
                     "socialists_unfavor_withDK",
                     "the_police_unfavor_withDK",
                     "undocumented_unfavor_withDK")

news_consumption <- c("news_sources_telemundo", "news_sources_other", 
                      "news_sources_npr", "news_sources_new_york_times", 
                      "news_sources_network", "news_sources_msnbc", 
                      "news_sources_localtv", "news_sources_local_newspaper", 
                      "news_sources_fox", "news_sources_facebook", "news_sources_cnn", 
                      "news_sources_amtalk")

# EXCLUDE PEOPLE WHO ARE NOT ELIGIBLE TO VOTE!
a_analysis <- a %>% filter(vote_intention != 3 ) %>%   
  select(outcome,         
         all_of(gender_vars),
         all_of(racial_att_vars),
         all_of(group_attitudes),
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
         Men,Black,Asian,
         Hispanic,
         age,
         pid7,
         ideo5,
         educ_category,
         inc_group,
         household_gun_owner,
         in_union,
         is_evangelical,
         language,
         orientation_group,
         sc_knowledge,
         sen_knowledge)


names(a_analysis)
# One-hot encoding
# SUBSETTING IS FRAGILE as we are relying here on the correct order of columns...
a_analysis[,which(names(a_analysis)=="pid7"):which(names(a_analysis)=="sen_knowledge")] <- lapply(a_analysis[,which(names(a_analysis)=="pid7"):which(names(a_analysis)=="sen_knowledge")] , factor)
Xm1 <- dummy_cols(a_analysis,remove_selected_columns=T)
Xm <- Xm1 %>% filter(complete.cases(.))

# This must come AFTER one-hot encoding the features,
# because we don't want to have two separate outcome variables: just keep a single binary variable.
Xm$biden20 <- factor(Xm$biden20, levels = c(0,1), labels = c("TrumpOrDK", "Biden"))

