library(tidyverse)
library(haven)
library(broom)

anes2020_temp <- read_dta("~/Dropbox/B_polls/ANES/2020panel/anes_timeseries_2020_stata_20210324.dta")

# anes2016_cumulative <- read_dta("data/anes/anes_timeseries_cdf.dta")
anes2016_cumulative <- read_dta("~/Dropbox/B_polls/ANES/cumulative_to_2016/anes_timeseries_cdf.dta")


                           

anes2016 <- anes2016_cumulative %>%
  transmute(
    year = VCF0004,
    college_grad = ifelse(VCF0110==4,1,0),
    votedRepublican2P = ifelse(VCF0704==2,1,
                               ifelse(VCF0704==1,0,NA)),
    votedDemocrat2P = ifelse(VCF0704==1,1,
                               ifelse(VCF0704==2,0,NA)),
    Republican = ifelse(VCF0301 %in% c(5:7),1,
                              ifelse(is.na(VCF0301),NA,0)),
    Democrat = ifelse(VCF0301 %in% c(1:3), 1,
                            ifelse(is.na(VCF0301),NA,0)),
    female = ifelse(VCF0104==2,1,0),
    white = ifelse(VCF0105a==1,1,0),
    black = ifelse(VCF0105a==2,1,0),
    income = VCF0114,
    age = VCF0102
  )


# Deal with missing values:
anes2020_temp$V201617x[anes2020_temp$V201617x==-9] <- NA
anes2020_temp$V201617x[anes2020_temp$V201617x==-5] <- NA
anes2020_temp$V201507x[anes2020_temp$V201507x==-9] <- NA


anes2020 <- anes2020_temp %>%
  transmute(year=2020,
        votedRepublican2P = ifelse(V202073==2,1,
                                       ifelse(V202073==1,0,NA)),
        votedDemocrat2P = ifelse(V202073==1,1,
                                     ifelse(V202073==2,0,NA)),
        white = ifelse(V201549x==1,1,0),
        black = ifelse(V201549x==2,1,0),
        college_grad = ifelse(V201510 == 6 | V201510 == 7 | V201510 ==8,1,0),
        Democrat = ifelse(V201231x >= 1 & V201231x <=3,1,0),
        female = ifelse(V201600==2,1,0),
        Republican = ifelse(V201231x==5 | V201231x==6 | V201231x==7,1,0),
        income = V201617x,
        age = V201507x
  )


anes_cumulative_to_2020 <- bind_rows(
  anes2016,
  anes2020
)

############
# PID MODELS
############
mod_2020_rep <- anes_cumulative_to_2020 %>% 
  filter(!is.na(Republican),
         !is.na(white),
         !is.na(female),
         !is.na(age),
         !is.na(income),
         !is.na(college_grad)) %>%
  group_by(year) %>%
  do(tidy(lm(Republican ~ 
               white + black +    
               college_grad +
               female + 
               factor(income) +
               age,
             data=. ))) 

mod_2020_rep_logit <- anes_cumulative_to_2020 %>% 
  filter(!is.na(Republican),
         !is.na(white),
         !is.na(female),
         !is.na(age),
         !is.na(income),
         !is.na(college_grad)) %>%
  group_by(year) %>%
  do(tidy(glm(Republican ~ 
               white + black +    
               college_grad +
               female + 
               factor(income) +
               age,
             data=. , 
             family = "binomial"))) 

########################
# VOTE CHOICE MODELS
########################

VOTE_mod_2020_rep <- anes_cumulative_to_2020 %>% 
  filter(!is.na(votedRepublican2P),
         !is.na(white),
         !is.na(female),
         !is.na(age),
         !is.na(income),
         !is.na(college_grad)) %>%
  group_by(year) %>%
  do(tidy(lm(votedRepublican2P ~ 
               white + black +    
               college_grad +
               female + 
               factor(income) +
               age,
             data=. ))) 

VOTE_mod_2020_rep_logit <- anes_cumulative_to_2020 %>% 
  filter(!is.na(votedRepublican2P),
         !is.na(white),
         !is.na(female),
         !is.na(age),
         !is.na(income),
         !is.na(college_grad)) %>%
  group_by(year) %>%
  do(tidy(glm(votedRepublican2P ~ 
                white + black +    
                college_grad +
                female + 
                factor(income) +
                age,
              data=. , 
              family = "binomial"))) 

#######
# PLOTS
#######

(
  F2_part2 <-  mod_2020_rep %>% 
  # filter(term != "(Intercept)",
  #        term != "black") %>%
  mutate(term2 = case_when(
                           term == "white" ~ "White",
                           term == "female" ~ "Woman",
                           term == "college_grad" ~ "College graduate")) %>%
  filter(!is.na(term2)) %>%
  ggplot(aes(x=year,y=estimate,color=term)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                    ymin = estimate - 1.96*std.error),width=.3) +
  facet_wrap(~term2) +
  labs(y="Estimated marginal effect\nof selected demographics", x = "ANES wave",
       subtitle = "Predicting Republican Party ID") +
  geom_hline(yintercept=0) +
  scale_x_continuous(breaks = c(1948,seq(1950,2020,10))) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=13)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(1948, 2020, by = 8))
)
ggsave("fig/ANES_OLS/LPM_demos_Outcome_is_RepubPID_upto2020.pdf")


mod_2020_rep_logit %>% 
  # filter(term != "(Intercept)",
  #        term != "black") %>%
  mutate(term2 = case_when(
    term == "white" ~ "White",
    term == "female" ~ "Woman",
    term == "college_grad" ~ "College graduate")) %>%
  filter(!is.na(term2)) %>%
  ggplot(aes(x=year,y=estimate,color=term)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                    ymin = estimate - 1.96*std.error),width=.3) +
  facet_wrap(~term2) +
  labs(y="Estimated logit coefficients\nof demographics", x = "ANES wave",
       subtitle = "Predicting Republican PID (X = race, education, gender, age, income)") +
  geom_hline(yintercept=0) +
  scale_x_continuous(breaks = c(1948,seq(1950,2020,10))) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=13)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(1948, 2020, by = 8))
ggsave("fig/ANES_OLS/LPM_demos_Outcome_is_RepubPID_logit.pdf")



(
  F2_part1 <- VOTE_mod_2020_rep %>% 
  # filter(term != "(Intercept)",
  #        term != "black") %>%
  mutate(term2 = case_when(
    term == "white" ~ "White",
    term == "female" ~ "Woman",
    term == "college_grad" ~ "College graduate")) %>%
  filter(!is.na(term2)) %>%
  ggplot(aes(x=year,y=estimate,color=term)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                    ymin = estimate - 1.96*std.error),width=.3) +
  facet_wrap(~term2) +
  labs(y="Estimated marginal effect\nof selected demographics", x = "ANES wave",
       subtitle = "Predicting Republican two-party vote choice") +
  geom_hline(yintercept=0) +
  scale_x_continuous(breaks = c(1948,seq(1950,2020,10))) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=13)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(1948, 2020, by = 8))
# (X = race, education, gender, age, income)
)
ggsave("fig/ANES_OLS/LPM_demos_Outcome_is_RepubVoteChoice_upto2020.pdf")


library(ggpubr)
ggarrange(F2_part1,F2_part2,nrow = 2)
ggsave("fig/ANES_OLS/LPM_demos_2Outcomes.pdf")
