library(tidyverse)
library(haven)
library(broom)
library(here)

# Import data
anes <- read_dta(here("data/anes/anes_timeseries_cdf_stata_20211118.dta"))

# Prepare data for specifications 1 and 2
anesRECODE <- anes %>%
  transmute(
    year = VCF0004,
    college_grad = ifelse(VCF0110 == 4, 1, 0),
    votedRepublican2P = ifelse(VCF0704 == 2, 1,
      ifelse(VCF0704 == 1, 0, NA)
    ),
    votedDemocrat2P = ifelse(VCF0704 == 1, 1,
      ifelse(VCF0704 == 2, 0, NA)
    ),
    Republican = ifelse(VCF0301 %in% c(5:7), 1,
      ifelse(is.na(VCF0301), NA, 0)
    ),
    Democrat = ifelse(VCF0301 %in% c(1:3), 1,
      ifelse(is.na(VCF0301), NA, 0)
    ),
    female = ifelse(VCF0104 == 2, 1, 0),
    white = ifelse(VCF0105a == 1, 1, 0),
    black = ifelse(VCF0105a == 2, 1, 0),
    income = VCF0114,
    age_group = VCF0102
  )

estimate_model <- function(df, outcome) {
  form <- as.formula(
    paste(
      outcome, " ~ ",
      "white + black + college_grad + female + ", 
      "factor(income) + factor(age_group)"
    )
  )
  glm(form, family = "binomial", data = df)
}

voteRepublican_logit <- anesRECODE %>%
  filter(!is.na(votedRepublican2P)) %>%
  filter(year >= 1972) %>%
  group_by(year) %>%
  nest() %>%
  # these are logit coeffients
  mutate(model = map(data, estimate_model, "votedRepublican2P")) 


# OLS regression 1:
voteRepublican <- anesRECODE %>%
  filter(!is.na(votedRepublican2P)) %>%
  group_by(year) %>%
  do(tidy(lm(votedRepublican2P ~
  white + black +
    college_grad +
    female +
    factor(income) +
    factor(age_group),
  data = .
  )))

# OLS regression 2:
identityRepublican <- anesRECODE %>%
  filter(year >= 1972) %>%
  filter(
    !is.na(Republican),
    !is.na(income)
  ) %>%
  group_by(year) %>%
  do(tidy(lm(Republican ~
  white + black +
    college_grad +
    female +
    factor(income) +
    factor(age_group),
  data = .
  )))


figure2_Vote <- voteRepublican %>%
  filter(year >= 1972) %>%
  mutate(term2 = case_when(
    term == "white" ~ "White",
    term == "female" ~ "Woman",
    term == "college_grad" ~ "College graduate"
  )) %>%
  filter(!is.na(term2)) %>%
  ggplot(aes(x = year, y = estimate, color = term, shape = term)) +
  geom_point(size = 2.3) +
  geom_errorbar(aes(
    ymax = estimate + 1.96 * std.error,
    ymin = estimate - 1.96 * std.error
  ), width = .3) +
  facet_wrap(~term2) +
  labs(
    y = "Estimated marginal effect\nof selected demographics", x = "ANES wave",
    subtitle = "Predicting Republican two-party vote choice"
  ) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(1976, seq(1984, 2020, 8), 2020)) +
  scale_color_manual(values = c("darkblue", "darkred", "darkorange")) +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 13)
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


figure2_RepPID <- identityRepublican %>%
  filter(year >= 1972) %>%
  mutate(term2 = case_when(
    term == "white" ~ "White",
    term == "female" ~ "Woman",
    term == "college_grad" ~ "College graduate"
  )) %>%
  filter(!is.na(term2)) %>%
  ggplot(aes(x = year, y = estimate, color = term, shape = term)) +
  geom_point(size = 2.3) +
  geom_errorbar(aes(
    ymax = estimate + 1.96 * std.error,
    ymin = estimate - 1.96 * std.error
  ), width = .3) +
  facet_wrap(~term2) +
  labs(
    y = "Estimated marginal effect\nof selected demographics", x = "ANES wave",
    subtitle = "Predicting Republican Party ID"
  ) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(1976, seq(1984, 2020, 8), 2020)) +
  scale_color_manual(values = c("darkblue", "darkred", "darkorange")) +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 13)
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggpubr::ggarrange(figure2_Vote, figure2_RepPID, nrow = 2)
ggsave("fig/Figure2.pdf")
