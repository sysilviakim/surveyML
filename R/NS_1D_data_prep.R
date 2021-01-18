# Prep dataset for Biden analysis

# Key aspects of the data prep:

#  - use eligible voters only
#  - TWO-party vote [e.g. Trump vote intent]
#  - narrow income categories (5)
#  - And use responses from July 2019 to Feb. 2020 b/c 
#    there appears to be data quality
#    ... deterioration during the Covid crisis.

library(tidyverse)
library(haven)
library(fastDummies)
library(ranger)

library(caret)

# ISSUES to note with the 2ns NS data  release [Sept. 2020]
##    Must change pid7 to  pid7_legacy
##    Better not to include Jabolish_priv_insurance [too many NAs]
##    Income is missing for ~15K respondents

a_release2 <- read_dta("data/nationscape/nationscape_CD_merged_SEP2020_v2.dta")

library(lubridate)
a_release2$date <- as.Date(a_release2$start_date)
a_release2$monthyear <- floor_date(a_release2$date, "month")
a_release2$week <- floor_date(a_release2$date, "week")
a_release2$MY <- month(a_release2$monthyear, label = TRUE)

a_release2 <- a_release2 %>% mutate(
  trump2Pvote_intent =
    case_when(
      trump_biden == 2 ~ "Trump",
      trump_biden == 1 ~ "Biden"
    )
)

# Choose your desired outcome
outcome <- "trump2Pvote_intent"

# Decide whether you want these sets of identity variables:
gender_vars <- c(
  "gender_attitudes_complain",
  "gender_attitudes_logical",
  "gender_attitudes_maleboss"
)
racial_att_vars <- c(
  "racial_attitudes_tryhard",
  "racial_attitudes_generations",
  "racial_attitudes_marry",
  "racial_attitudes_date",
  "discrimination_blacks",
  "discrimination_muslims",
  "discrimination_women",
  "discrimination_men",
  "discrimination_whites",
  "discrimination_christians"
)

group_attitudes <- c(
  "asians_favor_withDK",
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
  "undocumented_unfavor_withDK"
)

news_consumption <- c(
  "news_sources_telemundo", "news_sources_other",
  "news_sources_npr", "news_sources_new_york_times",
  "news_sources_network", "news_sources_msnbc",
  "news_sources_localtv", "news_sources_local_newspaper",
  "news_sources_fox", "news_sources_facebook", "news_sources_cnn",
  "news_sources_amtalk"
)

# Only registered 2P voters on or before Feb. 2020
a_analysis <- a_release2 %>%
  filter(
    date <= "2020-02-28",
    registered == 1,
    !is.na(trump_biden),
    trump_biden != 999
  ) %>% 
  # dropping undecided voters [just making sure, but note 
  # that outcome was defined carefully above to exclude undecided voters]
  select(
    outcome,
    monthyear,
    age,
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
    White,
    Men, Black, Asian,
    Hispanic,
    pid7_legacy,
    ideo5,
    educ_category,
    inc_group,
    household_gun_owner,
    in_union,
    is_evangelical,
    religion,
    language,
    orientation_group,
    sc_knowledge,
    sen_knowledge
  )


# devtools::install_github("sysilviakim/Kmisc")
# library(Kmisc)
# source("utilities.R")
#
#
# var_db <- data_routine(a_analysis %>% select(-monthyear),
#                        dep = "biden20", lvl = c(0, 1), lbl = c("Trump", "Biden"),
#                        dbl = "age", na = "NA")


# # Before Oct. 2020, JZ created factors using the code below:
# names(a_analysis)
# # One-hot encoding
# # SUBSETTING IS FRAGILE as we are relying here on the correct order of columns...
# a_analysis[,which(names(a_analysis)=="pid7_legacy"):which(names(a_analysis)=="sen_knowledge")] <- 
# lapply(a_analysis[,which(names(a_analysis)=="pid7_legacy"):which(names(a_analysis)=="sen_knowledge")] , factor)
# Xm1 <- dummy_cols(a_analysis,remove_selected_columns=T)
# Xm <- Xm1 %>% filter(complete.cases(.))

# This must come AFTER one-hot encoding the features,
# because we don't want to have two separate outcome variables:
# just keep a single binary variable.

# # Create factors/dummies
# Xm1 <- lapply(
#   a_analysis %>% select(-trump2Pvote_intent, -age, -monthyear), factor
# )
# Xm1 <- dummy_cols(Xm1, remove_selected_columns = T)
# 
# # This is only valid under some heroic assumptions, so comment it out
# ## Replace "NA" with zero (and note that "999" = "Not sure")
# # Xm1[is.na(Xm1)] <- 0
# 
# # Add back non-factor variables
# Xm <- Xm1 %>% 
#   bind_cols(a_analysis %>% select(trump2Pvote_intent, age, monthyear))
# Xm <- Xm %>% filter(complete.cases(.))

Xm <- bind_cols(
  a_analysis %>% 
    select(-trump2Pvote_intent, -age, -monthyear) %>%
    mutate_all(as.factor) %>%
    dummy_cols(remove_selected_columns = TRUE),
  a_analysis %>% 
    select(trump2Pvote_intent, age, monthyear) %>%
    mutate(trump2Pvote_intent = factor(trump2Pvote_intent))
) %>%
  filter(complete.cases(.))

# Xm$trump2Pvote_intent <- factor(
#   Xm$trump2Pvote_intent, labels = c("Biden", "Trump")
# )
