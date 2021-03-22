# summarise_models.R

library(tidyverse)
library(here)

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
summary(lm(L$auc ~ L$year))

mean(L$accuracy)