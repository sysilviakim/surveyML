# summarise_models.R

library(tidyverse)
library(here)

# Read in ANES + NS up to 2020:
L <- read_csv(here("tab","anes_logit_spec1.csv"))

L %>% pivot_longer(cols=2:3) %>%
  ggplot(aes(x=year, y=value)) +
  geom_point() +
  geom_line() +
  facet_grid(~name) +
  ylim(c(.5,.9)) +
  theme_bw()

L %>% 
  ggplot(aes(x=year, y=accuracy)) +
  geom_point() +
  geom_line() +
  ylim(c(.5,.9)) +
  # theme_bw() +
  labs(y="Accuracy", x="",
       title = "Accuracy from logit models based on demographics") +
  theme(text = element_text(size = 14))

ggsave("fig/Accuracy_1952_2020.pdf")

summary(lm(L$accuracy ~ L$year))
# P-val: 0.0914
summary(lm(L$auc ~ L$year))

mean(L$accuracy)
# 0.6427611

################
# A POOLED MODEL
################
P <- read_csv(here("tab","anes_logit_spec1_pooled.csv"))

P %>% pivot_longer(cols=3:4) %>%
  ggplot(aes(x=year, y=value, color = poll)) +
  geom_point() +
  geom_line() +
  facet_grid(~name) +
  ylim(c(.5,.9)) +
  theme_bw() +
  labs(y="Performance", x="",
       title = "Accuracy from logit models based on demographics",
       color = "") 

P %>% 
  ggplot(aes(x=year, y=accuracy, color = poll)) +
  geom_point(size=2) +
  geom_line() +
  ylim(c(.5,.9)) +
  theme_bw() +
  labs(y="Accuracy", x="",
       title = "Accuracy from logit models based on demographics",
       color = "") +
  theme(text = element_text(size = 16))
ggsave("fig/Accuracy_1952_2020_pooled.pdf")
  
summary(lm(P$accuracy ~ P$year))
# P-val: 0.101

summary(lm(P$auc ~ P$year))
# slope: 0.0018935, p<.01
summary(lm(auc ~ year, data = P %>% filter(year>=1964)))
# slope: 0.0010, p=.131