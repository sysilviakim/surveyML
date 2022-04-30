source(here::here("R", "utilities.R"))

# Import/export data ===========================================================
## 59,944 rows, 1,029(!) variables
anes <- read_dta(here("data/anes/anes_timeseries_cdf_stata_20211118.dta"))
anes_labels <- stata_varlabel_df(anes)

# Delete irrelevant variables ==================================================
anes <- anes %>%
  select(
    -(
      anes_labels %>%
        filter(
          grepl(
            paste0(
              "Version|Case ID|Weight:|Completion|Completion|Paper|Interview|",
              "Unique Respondent Number|Cross-section composition"
            ),
            label
          )
        ) %>%
        .$var
    )
  ) %>%
  ## limit the years of study
  filter(VCF0004 %in% anes_years) %>%
  mutate(
    VCF0130 = case_when(
      VCF0004 < 1972 ~ as.character(VCF0131),
      TRUE ~ as.character(VCF0130)
    )
  )

anes_labels <- stata_varlabel_df(anes)

# Variable Sets for Cumulative Data ============================================
vl <- list(
  ## "depvar", "gender", "race", "educ", "faminc", "birthyr", "urbanism"
  ## VCF0704a: Vote for President- Major Parties
  ## Approval rating: VCF0450
  ## House: VCF0736
  ## Senate: VCF0710 but not exactly
  ## VCF0105a: no Asian separately
  ## Age, not birth year
  ## Family income group percentiles, not amounts
  set1 = c(
    "depvar",
    "VCF0101", ## age
    "VCF0104", ## gender
    "VCF0105b", ## 4-category race/ethnicity summary
    "VCF0110", ## 4-category education
    "VCF0114" ## respondent family income group
  ),
  ## 7-point party ID
  set2 = c("VCF0301"),
  ## Issues
  ## Difficult to define for ANES, so using elimination
  ## Excluding "belief" variables e.g. VCF9223
  ## "How likely is it that recent immigration levels will take jobs away ..."
  ## Including "should be" questions
  set3 = anes_labels %>%
    filter(!grepl("contact|thermo|vot|weight|language", tolower(label))) %>%
    filter(!grepl("index|interview|composition", tolower(label))) %>%
    filter(!grepl("house cand|senate cand|respondent -", tolower(label))) %>%
    filter(!grepl("ethnicity|dislike|head of hh|post", tolower(label))) %>%
    filter(!grepl("party id|like about dem|like about rep", tolower(label))) %>%
    filter(!grepl("how well does|turnout|president- ", tolower(label))) %>%
    filter(!grepl("household - |scale placement|state", tolower(label))) %>%
    filter(!grepl("umber child|number of child|child in h", tolower(label))) %>%
    filter(!grepl("father|mother|recall", tolower(label))) %>%
    filter(!grepl("challenger to|incumbent", tolower(label))) %>%
    filter(!grepl("republican presidential cand", tolower(label))) %>%
    filter(!grepl("democratic presidential cand", tolower(label))) %>%
    filter(!grepl("house rep|house dem", tolower(label))) %>%
    filter(!grepl("senate rep|senate dem", tolower(label))) %>%
    filter(!grepl("year of study|version|census|county", tolower(label))) %>%
    filter(!grepl("newspap|respondent like|number of day", tolower(label))) %>%
    filter(!grepl("parent|race|completion", tolower(label))) %>%
    filter(!grepl("respondent family| id", tolower(label))) %>%
    filter(!grepl("likes anything|major party|scale", tolower(label))) %>%
    filter(!grepl("liberal|question|respondent level", tolower(label))) %>%
    filter(!grepl("which party|internet|stock|magazine", tolower(label))) %>%
    filter(!grepl("has r|how often|satisf|job or office", tolower(label))) %>%
    filter(!grepl("length of|cses|best handle|interest in", tolower(label))) %>%
    filter(!grepl("party with|party of|did r", tolower(label))) %>%
    filter(!grepl("many|life|would r|count of", tolower(label))) %>%
    filter(!grepl("how much better|how much attention", tolower(label))) %>%
    filter(!grepl("does r know|does r have|does r belong", tolower(label))) %>%
    filter(!grepl("does r get|does r think|respondent", tolower(label))) %>%
    filter(!grepl("branch|better or worse|better/worse", tolower(label))) %>%
    filter(!grepl("government level|performance rating", tolower(label))) %>%
    filter(!grepl("valid|election| better|urban|south", tolower(label))) %>%
    filter(!grepl("registration|child to|family|bible", tolower(label))) %>%
    filter(!grepl("world|approve/disapprove of|well", tolower(label))) %>%
    filter(!grepl("contribute|concern|party control|retro", tolower(label))) %>%
    filter(!grepl("complicate|residen|income|campaign", tolower(label))) %>%
    filter(!grepl("major part|church|eligible|helpful", tolower(label))) %>%
    filter(!grepl("constituent|likely|cooperative|trusted", tolower(label))) %>%
    filter(!grepl("gave money|does working", tolower(label))) %>%
    ## This excludes "Government Should Ensure Fair Jobs for Blacks"
    filter(!grepl("blacks ", tolower(label))) %>%
    .$var,
  ## Appendix materials:
  ## demo. + religion: church attendance recoded from 1970 on, so two sep. var
  set5 = c(
    "VCF0128", ## religion major group
    "VCF0131", ## church attendance 1952-1968
    "VCF0130" ## church attendance 1970~
  ),
  ## demo. + south
  set6 = "VCF0113",
  ## demo more comprehensive (R&R)
  set7 = c(
    "VCF0111", ## urban-suburban-rural
    "VCF0112", ## census region
    "VCF0113", ## political south/nonsouth
    "VCF0118", ## work status 5-category
    "VCF0128", ## religion major group
    ## "VCF0131", ## church attendance 1952-1968 ---> now pasted
    "VCF0130", ## church attendance 1970~
    "VCF0146", ## home ownership
    "VCF0147" ## marital status
  ),
  ## demo. + symbolic ideology, 3-pt
  set8 = "VCF0804"
)

## set8 can just re-use set3
vl$set9 <- vl$set3
save(vl, file = here("data", "anes-tidy", "anes-vl.Rda"))

# Sanity checks: are data present for each year? ===============================
table(anes$VCF0004, anes$VCF0101)
table(anes$VCF0004, anes$VCF0104)
table(anes$VCF0004, anes$VCF0105b)
table(anes$VCF0004, anes$VCF0110)
table(anes$VCF0004, anes$VCF0111) ## missing 2004, 2008, 2012, 2016, and 2020
table(anes$VCF0004, anes$VCF0112)
table(anes$VCF0004, anes$VCF0113)
table(anes$VCF0004, anes$VCF0114)
table(anes$VCF0004, anes$VCF0128)
table(anes$VCF0004, anes$VCF0130) ## by design starting 1972, so fixed manually
table(anes$VCF0004, anes$VCF0146)
table(anes$VCF0004, anes$VCF0147)
