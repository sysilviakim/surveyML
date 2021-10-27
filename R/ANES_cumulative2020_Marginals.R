source("R/ANES_cumulative_buildTo2020.R")

# The code above generates the dateset: anes_cumulative_to_2020

head(anes_cumulative_to_2020)

# We run rolling election-by-election regressions, and tidy the models
# in order to extract marginal effects:

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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
)
ggsave("fig/ANES_OLS/LPM_demos_Outcome_is_RepubPID_upto2020.pdf")

(
  F2_part2_1976plus <-  mod_2020_rep %>% 
    filter(year >=1976) %>%
    # filter(term != "(Intercept)",
    #        term != "black") %>%
    mutate(term2 = case_when(
      term == "white" ~ "White",
      term == "female" ~ "Woman",
      term == "college_grad" ~ "College graduate")) %>%
    filter(!is.na(term2)) %>%
    ggplot(aes(x=year,y=estimate,color=term, shape = term)) +
    geom_point(size=2.3) +
    geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                      ymin = estimate - 1.96*std.error),width=.3) +
    facet_wrap(~term2) +
    labs(y="Estimated marginal effect\nof selected demographics", x = "ANES wave",
         subtitle = "Predicting Republican Party ID") +
    geom_hline(yintercept=0) +
    scale_x_continuous(breaks = c(1976,seq(1984,2020,8),2020)) +
    scale_color_manual(values = c("darkblue","darkred","darkorange")) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size=13)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
)

(
  F2_part2_1976plus_logit <-  mod_2020_rep_logit %>% 
    filter(year >=1976) %>%
    # filter(term != "(Intercept)",
    #        term != "black") %>%
    mutate(term2 = case_when(
      term == "white" ~ "White",
      term == "female" ~ "Woman",
      term == "college_grad" ~ "College graduate")) %>%
    filter(!is.na(term2)) %>%
    ggplot(aes(x=year,y=estimate,color=term, shape = term)) +
    geom_point(size=2.3) +
    geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                      ymin = estimate - 1.96*std.error),width=.3) +
    facet_wrap(~term2) +
    labs(y="Logit coefficient", x = "ANES wave",
         subtitle = "Predicting Republican Party ID") +
    geom_hline(yintercept=0) +
    scale_x_continuous(breaks = c(1976,seq(1984,2020,8),2020)) +
    scale_color_manual(values = c("darkblue","darkred","darkorange")) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size=13)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
)



# mod_2020_rep_logit %>% 
#   # filter(term != "(Intercept)",
#   #        term != "black") %>%
#   mutate(term2 = case_when(
#     term == "white" ~ "White",
#     term == "female" ~ "Woman",
#     term == "college_grad" ~ "College graduate")) %>%
#   filter(!is.na(term2)) %>%
#   ggplot(aes(x=year,y=estimate,color=term)) +
#   geom_point() +
#   geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
#                     ymin = estimate - 1.96*std.error),width=.3) +
#   facet_wrap(~term2) +
#   labs(y="Estimated logit coefficients\nof demographics", x = "ANES wave",
#        subtitle = "Predicting Republican PID (X = race, education, gender, age, income)") +
#   geom_hline(yintercept=0) +
#   scale_x_continuous(breaks = c(1948,seq(1950,2020,10))) +
#   theme_bw() +
#   theme(legend.position = "none",
#         text = element_text(size=13)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggsave("fig/ANES_OLS/LPM_demos_Outcome_is_RepubPID_logit.pdf")



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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# (X = race, education, gender, age, income)
)
ggsave("fig/ANES_OLS/LPM_demos_Outcome_is_RepubVoteChoice_upto2020.pdf")


(
  F2_part1_1976plus <- VOTE_mod_2020_rep %>% 
    filter(year >= 1976) %>%
    # filter(term != "(Intercept)",
    #        term != "black") %>%
    mutate(term2 = case_when(
      term == "white" ~ "White",
      term == "female" ~ "Woman",
      term == "college_grad" ~ "College graduate")) %>%
    filter(!is.na(term2)) %>%
    ggplot(aes(x=year,y=estimate,color=term, shape = term)) +
    geom_point(size=2.3) +
    geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                      ymin = estimate - 1.96*std.error),width=.3) +
    facet_wrap(~term2) +
    labs(y="Estimated marginal effect\nof selected demographics", x = "ANES wave",
         subtitle = "Predicting Republican two-party vote choice") +
    geom_hline(yintercept=0) +
    scale_x_continuous(breaks = c(1976,seq(1984,2020,8),2020)) +
    scale_color_manual(values = c("darkblue","darkred","darkorange")) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size=13)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  # (X = race, education, gender, age, income)
)

(
  F2_part1_1976plus_logit <- VOTE_mod_2020_rep_logit %>% 
    filter(year >= 1976) %>%
    # filter(term != "(Intercept)",
    #        term != "black") %>%
    mutate(term2 = case_when(
      term == "white" ~ "White",
      term == "female" ~ "Woman",
      term == "college_grad" ~ "College graduate")) %>%
    filter(!is.na(term2)) %>%
    ggplot(aes(x=year,y=estimate,color=term, shape = term)) +
    geom_point(size=2.3) +
    geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                      ymin = estimate - 1.96*std.error),width=.3) +
    facet_wrap(~term2) +
    labs(y="Logit coefficient", x = "ANES wave",
         subtitle = "Predicting Republican two-party vote choice") +
    geom_hline(yintercept=0) +
    scale_x_continuous(breaks = c(1976,seq(1984,2020,8),2020)) +
    scale_color_manual(values = c("darkblue","darkred","darkorange")) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size=13)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  # (X = race, education, gender, age, income)
)



library(ggpubr)
ggarrange(F2_part1,F2_part2,nrow = 2)
ggsave("fig/ANES_OLS/LPM_demos_2Outcomes.pdf")

ggarrange(F2_part1_1976plus,F2_part2_1976plus,nrow = 2)
ggsave("fig/ANES_OLS/LPM_demos_2Outcomes-shorter.pdf")

ggarrange(F2_part1_1976plus,F2_part2_1976plus,nrow = 2)
ggsave("fig/ANES-logit-shorter.pdf")

ggarrange(F2_part1_1976plus_logit,F2_part2_1976plus_logit,nrow = 2)
ggsave("fig/ANES_OLS/LPM-fig-shorter.pdf")
