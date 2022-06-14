source(here::here("R", "utilities.R"))
library(broom)

# Import data ==================================================================
anes <-
  haven::read_dta(here("data/anes/anes_timeseries_cdf_stata_20211118.dta")) %>%
  filter(VCF0004 >= 1952)

# "Traditional" logit model: prepare data ======================================
anes_recode <- anes %>%
  transmute(
    year = VCF0004,
    votedRepublican2P = ifelse(VCF0704 == 2, 1, ifelse(VCF0704 == 1, 0, NA)),
    # votedDemocrat2P = ifelse(VCF0704 == 1, 1, ifelse(VCF0704 == 2, 0, NA)),
    Republican = case_when(
      !is.na(VCF0301) & VCF0301 %in% seq(5, 7) ~ 1,
      !is.na(VCF0301) & VCF0301 %in% seq(3) ~ 0
    ),
    # Democrat = ifelse(VCF0301 %in% c(1:3), 1, ifelse(is.na(VCF0301), NA, 0)),
    age = VCF0101,
    gender = case_when(
      !is.na(VCF0104) & VCF0104 == 0 ~ "na",
      !is.na(VCF0104) & VCF0104 == 1 ~ "male",
      !is.na(VCF0104) & VCF0104 == 2 ~ "female",
      !is.na(VCF0104) & VCF0104 == 3 ~ "other",
      is.na(VCF0104) ~ "missing"
    ),
    gender = factor(
      gender,
      levels = c("na", "male", "female", "other", "missing")
    ),
    race = case_when(
      !is.na(VCF0105b) & VCF0105b == 0 ~ "na",
      !is.na(VCF0105b) & VCF0105b == 1 ~ "white",
      !is.na(VCF0105b) & VCF0105b == 2 ~ "black",
      !is.na(VCF0105b) & VCF0105b == 3 ~ "hispanic",
      !is.na(VCF0105b) & VCF0105b == 4 ~ "other race",
      !is.na(VCF0105b) & VCF0105b == 9 ~ "dk",
      is.na(VCF0105b) ~ "missing"
    ),
    race = factor(
      race,
      levels = c("na", "black", "white", "hispanic", "other race", "dk")
    ),
    income = case_when(
      is.na(VCF0114) ~ 999,
      TRUE ~ as.numeric(VCF0114)
    ),
    edu_coarse = case_when(
      !is.na(VCF0110) & VCF0110 == 4 ~ "Univ. graduate+",
      !is.na(VCF0110) & VCF0110 <= 3 ~ "Non-college"
    ),
    edu = case_when(
      !is.na(VCF0110) & VCF0110 == 0 ~ "na",
      !is.na(VCF0110) & VCF0110 == 1 ~ "less than high",
      !is.na(VCF0110) & VCF0110 == 2 ~ "high school",
      !is.na(VCF0110) & VCF0110 == 3 ~ "some college",
      !is.na(VCF0110) & VCF0110 == 4 ~ "college or higher",
      is.na(VCF0110) ~ "missing"
    ),
    edu = factor(
      edu,
      levels = c(
        "na", "less than high", "high school", "some college",
        "college or higher"
      )
    )
  ) %>%
  filter(!is.na(age))

anes_recode %>%
  map_dbl(~ sum(is.na(.x)))
#     year votedRepublican2P        Republican               age
#        0             35868              8873                 0
#   gender              race            income               edu
#        0                 0                 0                 0

estimate_model <- function(df, outcome, lpm = FALSE) {
  form <- as.formula(
    paste(
      outcome, " ~ ",
      "age + factor(gender) + factor(race) + factor(income) + factor(edu_coarse)"
    )
  )
  if (lpm) {
    lm(form, data = df)
  } else {
    glm(form, family = "binomial", data = df)
  }
}

# OLS regression 1 (vote choice) ===============================================
vote_republican <- anes_recode %>%
  select(-Republican) %>%
  filter(!is.na(votedRepublican2P)) %>%
  group_by(year)

assert_that(!any(is.na(vote_republican)))
vote_republican <- vote_republican %>%
  do(tidy(estimate_model(., "votedRepublican2P", lpm = TRUE)))

# OLS regression 2 (binary PID) ================================================
identity_republican <- anes_recode %>%
  select(-votedRepublican2P) %>%
  filter(!is.na(Republican)) %>%
  filter(year != 2002) %>%
  ## no income
  group_by(year)

assert_that(!any(is.na(identity_republican)))
identity_republican <- identity_republican %>%
  do(tidy(estimate_model(., "Republican", lpm = TRUE)))

# Figure creation ==============================================================
p <- list(
  vote = vote_republican,
  pid = identity_republican
) %>%
  imap(
    ~ .x %>%
      ## For readability
      filter(year >= 1976) %>%
      mutate(
        term2 = case_when(
          term == "factor(race)white" ~ "White",
          term == "factor(gender)female" ~ "Woman",
          term == "factor(edu_coarse)Univ. graduate+" ~ "College Graduate"
        ),
        term2 = factor(term2, levels = c("College Graduate", "White", "Woman"))
      ) %>%
      filter(!is.na(term2)) %>%
      ggplot(aes(x = year, y = estimate, color = term2, shape = term2)) +
      geom_point(size = 2.3) +
      geom_errorbar(
        aes(
          ymax = estimate + 1.96 * std.error,
          ymin = estimate - 1.96 * std.error
        ),
        width = .3
      ) +
      facet_wrap(~term2) +
      labs(
        color = "", shape = "",
        y = "Estimated Marginal Effect of\nSelected Demographics",
        x = "ANES Wave",
        subtitle = case_when(
          .y == "vote" ~ "Predicting Republican Two-party Vote Choice",
          .y == "pid" ~ "Predicting Republican Party ID"
        )
      ) +
      geom_hline(yintercept = 0) +
      scale_x_continuous(breaks = seq(1976, 2020, by = 4)) +
      scale_color_manual(values = c("darkblue", "darkorange", "darkred"))
  )

# Export =======================================================================
pdf(here("fig", "marginal_effects_lpm_edu_coarse.pdf"), width = 7, height = 6)
print(
  ggpubr::ggarrange(
    plot_nolegend(pdf_default(p$vote)) +
      theme(
        legend.position = "none",
        text = element_text(size = 13)
      ) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      xlab(""),
    plot_nolegend(pdf_default(p$pid)) +
      theme(
        legend.position = "none",
        text = element_text(size = 13)
      ) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)),
    nrow = 2
  )
)
dev.off()
