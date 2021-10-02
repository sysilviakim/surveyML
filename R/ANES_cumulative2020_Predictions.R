source("R/ANES_cumulative_buildTo2020.R")
# The code above generates the dateset: anes_cumulative_to_2020
head(anes_cumulative_to_2020)

# PURPOSE OF THE SCRIPT: Calculate Percent Correctly Predicted (PCP) respondents

library(DAMisc)
getPCP <- function(x) {
  DAMisc::pre(x)$pcp %>% round(3) *100
}

# We run rolling election-by-election regressions, and store the models
# (instead of tidying them)


########################
# VOTE CHOICE MODELS
########################

fit_REP <- function(df, var = c(" white + black +    
                        college_grad +
                        female + 
                        factor(income) +
                        age")) {
  form <- as.formula(paste("votedRepublican2P ~", var))
  glm(form, data = df, family = "binomial")
}

# Estimate a list of models:
VOTE_rep <- anes_cumulative_to_2020 %>% 
  filter(!is.na(votedRepublican2P),
         !is.na(white),
         !is.na(female),
         !is.na(age),
         !is.na(income),
         !is.na(college_grad)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(model = map(.x = data, .f = fit_REP),
         mod_pcp = map(model, getPCP))


# Extract accuracy from the list:
########################################
VOTE_rep_PCP <- VOTE_rep %>%
  dplyr::select(mod_pcp) %>%
  pivot_longer(cols=c(mod_pcp)) %>%
  unnest(value) 


# Display accuracy over time:
########################################
VOTE_rep_PCP %>% ggplot(aes(x=year,y=value)) +
  geom_point() +
  geom_line() +
  ylim(c(50,90)) +
  theme_bw() +
  labs(x="", y = "Accuracy",
       title = "Percent correctly predicted (two-party) votes",
       subtitle = "Logit-based predictions (using all available data)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(1948, 2020, by = 4))
ggsave("fig/ANES/ANES_accuracy_logit.pdf")



# If anyone needs R^2 (from an OLS model):
VOTE_rep_rSqaured <- anes_cumulative_to_2020 %>% 
  filter(!is.na(votedRepublican2P),
         !is.na(white),
         !is.na(female),
         !is.na(age),
         !is.na(income),
         !is.na(college_grad)) %>%
  group_by(year) %>%
  do(broom::glance(lm(votedRepublican2P ~ 
               white + black +    
               college_grad +
               female + 
               factor(income) +
               age,
             data=. )))
