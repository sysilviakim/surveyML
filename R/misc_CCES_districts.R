library(tidyverse)
library(haven)
library(broom)
library(here)

# Import data
cces <- read_dta("data/cces/cumulative_2006-2021.dta") %>%
  filter(year %in% c(2010:2020))

cces <- cces %>%
  mutate(
    CD = case_when(
      year %in% c(2013:2020) ~ cd,
      year %in% c(2012) ~ cd_up,
    )
  )
#cces %>% filter(!is.na(CD)) %>% tally()

districtDensity <- readr::read_csv("https://raw.githubusercontent.com/theatlantic/citylab-data/master/citylab-congress/citylab_cdi.csv") %>%
  rename(district_density = Cluster)

# Merge data
cces <- left_join(cces,districtDensity)

# Recode data
cces <- cces %>%
  mutate(
    democrat = ifelse(pid3_leaner==1,1,0),
    republican = ifelse(pid3_leaner==2,1,0),
    female = ifelse(gender==2,1,0),
    college_grad = ifelse(educ==5 | educ==6,1,0),
    white = ifelse(race==1,1,0),
    black = ifelse(race==2,1,0),
    pres_voteRep = ifelse(voted_pres_party==2,1,0), 
    pres_voteRep2P = ifelse(voted_pres_party==2,1,
                            ifelse(voted_pres_party==1,0,NA)), 
    pres_voteDem = ifelse(voted_pres_party==1,1,0)
  )

# Define functions
estimate_Spec1 <- function(df, outcome) {
  form <- as.formula(paste(outcome ," ~ ", "white + black + college_grad + female + age + factor(faminc)"))
  glm(form, family = "binomial", data = df)
}

estimate_modelWithDistricts <- function(df, outcome) {
  form <- as.formula(paste(outcome ," ~ ", " factor(district_density) + white + black + college_grad + female + age + factor(faminc)"))
  glm(form, family = "binomial", data = df)
}

library(DAMisc)
getPCP <- function(x) {
  DAMisc::pre(x)$pcp %>% round(3) *100
}


voteRepublican <- cces %>% 
  filter(!is.na(pres_voteRep)) %>%
  filter(!is.na(district_density)) %>%
  group_by(year) %>%
  do(tidy(lm(pres_voteRep2P ~
               factor(district_density) +
               white + black +    
               college_grad +
               female + age +
               factor(faminc),
             data=. ))) 

voteRepublican   %>% 
  filter(grepl('district',term)) %>%
  mutate(dist = substr(term,25,45)) %>%
  ggplot(aes(x=year,y=estimate,shape = dist)) +
  geom_point(size=2.3) +
  geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                    ymin = estimate - 1.96*std.error),width=.3) +
  facet_wrap(~dist) +
  scale_x_continuous(breaks = c(seq(2010,2020,2))) +
  labs(y="Estimated marginal effect of place", x = "Year",
       subtitle = "Predicting Republican two-party vote choice (in the latest election)") +
  geom_hline(yintercept=0) +
  scale_color_manual(values = c("darkblue","darkred","darkorange")) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=13)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("~/Downloads/CCES-district-density-types.pdf")

#########################
# Get accuracy over time
#########################
acc_rep2P <- cces %>%
  filter(!is.na(district_density)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(model_dist = map(.x = data, .f = estimate_modelWithDistricts,"pres_voteRep2P"),
         model_Spec1 = map(.x = data, .f = estimate_Spec1,"pres_voteRep2P"),
         `Demo. + Urban/Rural` = map(model_dist, getPCP),
         `Demographics` = map(model_Spec1, getPCP)
         )


VOTE_rep_PCP <- acc_rep2P %>%
  dplyr::select(contains("Demo")) %>%
  pivot_longer(cols=contains("Demo")) %>%
  unnest(value)

VOTE_rep_PCP %>%
  ggplot(aes(x=year, y = value, linetype = name)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(seq(2010,2020,2))) +
  labs(title ="Accuracy of 2P vote choice (logit modes, CCES, 2 specifications)",
       linetype = "Specification",
       x= "Year",
       y = "Accuracy")
ggsave("~/Downloads/CCES-accuracy-density.pdf")
