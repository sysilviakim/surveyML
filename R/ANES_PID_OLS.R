library(tidyverse)
library(haven)
library(broom)

anes <- read_dta("data/anes/anes_timeseries_cdf.dta")
# anes <- read_dta("~/Dropbox/B_polls/ANES/cumulative_to_2016/anes_timeseries_cdf.dta")

anes$year <- anes$VCF0004

anes$college_grad <- ifelse(anes$VCF0110==4,1,0)
anes$Republican <- ifelse(anes$VCF0301 %in% c(5:7),1,
                          ifelse(is.na(anes$VCF0301),NA,0))
anes$Democrat <- ifelse(anes$VCF0301 %in% c(1:3), 1,
                        ifelse(is.na(anes$VCF0301),NA,0))
                        
anes$female <- ifelse(anes$VCF0104==2,1,0)

anes$white <- ifelse(anes$VCF0105a==1,1,0)
anes$black <- ifelse(anes$VCF0105a==2,1,0)
anes$White <- ifelse(anes$VCF0106==1,1,0)
anes$Black <- ifelse(anes$VCF0106==2,1,0)

anes$state_fips <- anes$VCF0901a
anes$state_abb <- anes$VCF0901b

anes <- anes %>% mutate(incumbent = case_when(year %in% c(1984,1988,1992,2004,2008) ~ "Republican incumbent",
                                              year %in% c(1980,1996,2000,2012,2016) ~ "Democratic incumbent"))
                        # incumbent_party_vote = case_when(year %in% c(1972,1976,1984,1988,1992,2004,2008) ~ voted_republican,
                        #                                  year %in% c(1968,1980,1996,2000,2012,2016) ~ voted_democrat))


##################################################################
# RUN ROLLING REGRESSIONS,
# AND STORE COEFFICIENTS FOR EACH ELECTION YEAR
##################################################################

# Y = The respondent is Republican
#################################
mod_rep <- anes %>% 
  filter(!is.na(Republican),
         !is.na(white),
         !is.na(college_grad),
         !is.na(factor(VCF0114)),
         !is.na(factor(VCF0102))) %>%
  group_by(year) %>%
  do(tidy(lm(Republican ~ 
               white + black +    
                factor(VCF0114) + # income group [top category: 96 to 100 percentile]
                  factor(VCF0102) + # age 
                  female +
               college_grad,
             data=.))) 

# Y = The respondent is Democrat
#################################
mod_dem <- anes %>% 
  filter(!is.na(Democrat),
         !is.na(white),
         !is.na(college_grad),
         !is.na(factor(VCF0114)),
         !is.na(factor(VCF0102))) %>%
  group_by(year) %>%
  do(tidy(lm(Democrat ~ 
               white + black +    
               factor(VCF0114) + # income group [top category: 96 to 100 percentile]
               factor(VCF0102) + # age 
               female +
               college_grad,
             data=.))) 


#################################
# GENERATE COEFFICIENT PLOTS
#################################

mod_rep %>% 
  filter(term %in% c("black","white","college_grad","female")) %>%
  mutate(term2 = case_when(term == "black" ~ "Black",
                           term == "white" ~ "White",
                           term == "female" ~ "Woman",
                           term == "college_grad" ~ "College graduate")) %>%
  ggplot(aes(x=year,y=estimate,color=term)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                    ymin = estimate - 1.96*std.error),width=.3) +
  facet_wrap(~term2) +
  labs(y="Estimated marginal effect\nof demographics", x = "ANES wave",
       subtitle = "Predicting Republican PID (X = race, gender, college-grad status, income, age)") +
  geom_hline(yintercept=0) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=13))
ggsave("fig/ANES_OLS/LPM_demos_Outcome_is_RepubPID.pdf")


mod_dem %>%
  # filter(term != "(Intercept)") %>%
  filter(term %in% c("black","white","college_grad","female")) %>%
  mutate(term2 = case_when(term == "black" ~ "Black",
                           term == "white" ~ "White",
                           term == "female" ~ "Woman",
                           term == "college_grad" ~ "College graduate")) %>%
  ggplot(aes(x=year,y=estimate,color=term)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                    ymin = estimate - 1.96*std.error),width=.3) +
  facet_wrap(~term2) +
  labs(y="Estimated marginal effect\nof demographics", x = "ANES wave",
       subtitle = "Predicting Democratic PID (X = race, gender, college-grad status, income, age)") +
  geom_hline(yintercept=0) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=13))
ggsave("fig/ANES_OLS/LPM_demos_Outcome_is_DemocratPID.pdf")

mod_dem %>%
  # filter(term != "(Intercept)") %>%
  filter(term %in% c("black","white","college_grad","female"),
         year >=1980) %>%
  mutate(term2 = case_when(term == "black" ~ "Black",
                           term == "white" ~ "White",
                           term == "female" ~ "Woman",
                           term == "college_grad" ~ "College graduate")) %>%
  ggplot(aes(x=year,y=estimate,color=term)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                    ymin = estimate - 1.96*std.error),width=.3) +
  facet_wrap(~term2) +
  labs(y="Estimated marginal effect\nof demographics", x = "ANES wave",
       subtitle = "Predicting Democratic PID (X = race, gender, college-grad status, income, age)") +
  geom_hline(yintercept=0) +
  scale_x_continuous(breaks = c(seq(1980,2016,4))) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=13))
ggsave("fig/ANES_OLS/LPM_demos_Outcome_is_DemocratPID_1980onward.pdf")
