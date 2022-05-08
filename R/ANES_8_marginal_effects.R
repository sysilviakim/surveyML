source(here::here("R", "utilities.R"))
library(broom)

# Import data ==================================================================
anes <- read_dta(here("data/anes/anes_timeseries_cdf_stata_20211118.dta"))

# "Traditional" logit model: prepare data ======================================
anes_recode <- anes %>%
  transmute(
    year = VCF0004,
    votedRepublican2P = ifelse(VCF0704 == 2, 1, ifelse(VCF0704 == 1, 0, NA)),
    votedDemocrat2P = ifelse(VCF0704 == 1, 1, ifelse(VCF0704 == 2, 0, NA)),
    Republican = ifelse(VCF0301 %in% c(5:7), 1, ifelse(is.na(VCF0301), NA, 0)),
    Democrat = ifelse(VCF0301 %in% c(1:3), 1, ifelse(is.na(VCF0301), NA, 0)),
    age = VCF0101,
    female = ifelse(VCF0104 == 2, 1, 0),
    white = ifelse(VCF0105a == 1, 1, 0),
    black = ifelse(VCF0105a == 2, 1, 0),
    hispanic = ifelse(VCF0105a == 3, 1, 0),
    asian = ifelse(VCF0105a == 4, 1, 0),
    income = VCF0114,
    high_school = ifelse(VCF0110 == 2, 1, 0),
    some_college = ifelse(VCF0110 == 3, 1, 0),
    college_grad = ifelse(VCF0110 == 4, 1, 0),
    pid7 = VCF0301
  ) %>%
  filter(year >= 1952)

estimate_model <- function(df, outcome, lpm = FALSE) {
  form <- as.formula(
    paste(
      outcome, " ~ ",
      "age + female + black + hispanic + asian + ", 
      "high_school + some_college + college_grad + ",
      "factor(income)"
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
  filter(!is.na(votedRepublican2P) & !is.na(income)) %>%
  group_by(year) %>%
  do(tidy(estimate_model(., "votedRepublican2P", lpm = TRUE)))

# OLS regression 2 (binary PID) ================================================
identity_republican <- anes_recode %>%
  filter(year != 1954) %>%
  ## no data on variables such as race
  filter(pid7 %in% seq(7)) %>%
  filter(!is.na(Republican) & !is.na(income)) %>%
  group_by(year) %>%
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
          term == "black" ~ "Black",
          term == "female" ~ "Woman",
          term == "college_grad" ~ "College Graduate"
        ),
        term2 = factor(term2, levels = c("College Graduate", "Black", "Woman"))
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
pdf(here("fig", "marginal_effects_lpm.pdf"), width = 7, height = 6)
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
