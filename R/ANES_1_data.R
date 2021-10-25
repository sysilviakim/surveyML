source(here::here("R", "utilities.R"))

# Import/export data ===========================================================
## 59,944 rows, 1,029(!) variables
anes <- read_dta(here("data/anes/anes_timeseries_cdf.dta"))
anes_2020 <- read_dta(here("data/anes/anes_timeseries_2020_stata_20210719.dta"))

anes_labels <- stata_varlabel_df(anes)
anes_labels_2020 <- stata_varlabel_df(anes_2020)

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
  )
anes_labels <- stata_varlabel_df(anes)

## Delete all randomization and restricted variables from 2020
anes_2020 <- anes_2020 %>%
  select(
    -(
      anes_labels_2020 %>%
        filter(
          grepl(
            paste0(
              "version|RANDOM|RESTRICTED|Full sample|Case ID| weight|Sample|",
              "Corrections|Full sample|variance|Web-only|Mixed|mixed|",
              "All sample|fresh sample|POST ADMIN|PRE ADMIN|audio consent|",
              "participant booklet|Case validation|Completed pre only|",
              "Mode of interview|CAND: |SAMPLE: |SCREENER|IWR DESCR|IWR OBS|",
              "preloaded|Version|Audio recording consent|INTERVIEWER|",
              "Ease of online access"
            ),
            label
          )
        ) %>%
        .$var
    )
  )
anes_labels_2020 <- stata_varlabel_df(anes_2020)

## To match cumulative, need to create 3-pt ideo, South, and 4-category edu
anes_2020 <- anes_2020 %>%
  mutate(
    V201200b = case_when(
      V201200 %in% c(1, 2, 3) ~ 1,
      V201200 == 4 ~ 2,
      V201200 %in% c(5, 6, 7) ~ 3,
      TRUE ~ 9
    ),
    south = case_when(
      V201014b %in% (state_df %>% filter(south == 1) %>% .$stfips) ~ 1,
      TRUE ~ 0
    ),
    V201510 = as.numeric(V201510),
    V201510 = case_when(
      V201510 %in% c(4, 5, 6, 7, 8) ~ 4,
      TRUE ~ V201510
    )
  )

save(anes_2020, file = here("data/anes-tidy/anes_2020.Rda"))

# Variable Sets for Cumulative Data ============================================
vl <- list(
  ## "depvar", "gender", "race", "educ", "faminc", "birthyr"
  ## VCF0704a: Vote for President- Major Parties
  ## Approval rating: VCF0450
  ## House: VCF0736
  ## Senate: VCF0710 but not exactly
  ## VCF0105a: no Asian separately
  ## Age, not birth year
  ## Family income group percentiles, not amounts
  set1 = c("depvar", "VCF0104", "VCF0105b", "VCF0110", "VCF0114", "VCF0101"),
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
  set5 = c("VCF0128", "VCF0131", "VCF0130"),
  ## demo. + south
  set6 = "VCF0113",
  ## demo. + symbolic ideology, 3-pt
  set7 = "VCF0804"
)

## set8 can just re-use set3
vl$set8 <- vl$set3
save(vl, file = here("data", "anes-tidy", "anes-vl.RData"))

# Variable Sets for 2020 Data ==================================================
vl <- list(
  ## "depvar", "gender", "race", "educ", "faminc", "birthyr"
  ## VCF0704a: Vote for President- Major Parties
  ## Approval rating: VCF0450
  ## House: VCF0736
  ## Senate: VCF0710 but not exactly
  ## VCF0105a: no Asian separately
  ## Age, not birth year
  ## Family income group percentiles, not amounts
  set1 = c("depvar", "V201600", "V201549x", "V201510", "V201617x", "V201507x"),
  ## 7-point party ID
  set2 = c("V201231x"),
  ## Issues
  ## Difficult to define for ANES, so using elimination
  ## Excluding "belief" variables e.g. VCF9223
  ## "How likely is it that recent immigration levels will take jobs away ..."
  ## Including "should be" questions
  set3 = anes_labels_2020 %>%
    filter(!grepl("ballot card|birthdate|marital status", tolower(label))) %>%
    filter(!grepl("Mention: Radio PROG|Mention: TV PROG", label)) %>%
    filter(!grepl("Mention: Website|Order of THERMGR", label)) %>%
    filter(!grepl("order of|feels about how things are", tolower(label))) %>%
    filter(!grepl("gubernatorial candidate", tolower(label))) %>%
    filter(!grepl("contact|thermo|vot|language", tolower(label))) %>%
    filter(!grepl("house cand|senate cand|respondent", tolower(label))) %>%
    filter(!grepl("ethnicity|dislike|party id|like about", tolower(label))) %>%
    filter(!grepl("how well does|turnout|president -", tolower(label))) %>%
    filter(!grepl("household address|state of registrati", tolower(label))) %>%
    filter(!grepl("scale liberal-conservative", tolower(label))) %>%
    filter(!grepl("children in hh|mother|recall|incumbent", tolower(label))) %>%
    filter(!grepl("republican presidential cand", tolower(label))) %>%
    filter(!grepl("democratic presidential cand", tolower(label))) %>%
    filter(!grepl("house rep|house dem", tolower(label))) %>%
    filter(!grepl("senate rep|senate dem", tolower(label))) %>%
    filter(!grepl("county|newspap|respondent|number of", tolower(label))) %>%
    filter(!grepl("native status of parent|how many grand", tolower(label))) %>%
    filter(!grepl("feeling thermometer|has r|how often", tolower(label))) %>%
    filter(!grepl("preference for|party with|party of", tolower(label))) %>%
    filter(!grepl("did r|race|completion|liberal|stock", tolower(label))) %>%
    filter(!grepl("(family) income|how many family", tolower(label))) %>%
    filter(!grepl("family or friends|family/neighbors", tolower(label))) %>%
    filter(!grepl("relationships w/family|immediate famil", tolower(label))) %>%
    filter(!grepl("family members stopped by police", tolower(label))) %>%
    filter(!grepl("question|level of|which party|internet", tolower(label))) %>%
    filter(!grepl("satisf|handling job|handling its job", tolower(label))) %>%
    filter(!grepl("lost jobs|r contribute money|cses", tolower(label))) %>%
    filter(!grepl("interested in|many|life experience", tolower(label))) %>%
    filter(!grepl("would r|some other country|what is r's", tolower(label))) %>%
    filter(!grepl("how much better|how much is", tolower(label))) %>%
    filter(!grepl("how much approve/disapprove", tolower(label))) %>%
    filter(!grepl("how important is|registration state", tolower(label))) %>%
    filter(!grepl("health of r|tested pos for|media", tolower(label))) %>%
    filter(!grepl("how long|reason|how much influence", tolower(label))) %>%
    filter(!grepl("hich among mentions is the most import", tolower(label))) %>%
    filter(!grepl("materialism|does r have|does r get", tolower(label))) %>%
    filter(!grepl("does r think|economy better or worse", tolower(label))) %>%
    filter(!grepl("unemployment better or worse|prefer ", tolower(label))) %>%
    filter(!grepl("is the us better|split government", tolower(label))) %>%
    filter(!grepl(" rating|concentrating|class", tolower(label))) %>%
    filter(!grepl("presidential election|trust election o", tolower(label))) %>%
    filter(!grepl("election if covid|russian interference", tolower(label))) %>%
    filter(!grepl("working status|stereotype|child trait", tolower(label))) %>%
    filter(!grepl("urban area|urban person|registration", tolower(label))) %>%
    filter(!grepl("bible|(family) income|well|contribute", tolower(label))) %>%
    filter(!grepl("discrimination in|world around them", tolower(label))) %>%
    filter(!grepl("world temperat|world weaker or strong", tolower(label))) %>%
    filter(!grepl("concern|party control|complicate", tolower(label))) %>%
    filter(!grepl("resident|campaign|major part|church", tolower(label))) %>%
    filter(
      !(grepl("income", tolower(label)) &
        !grepl("inequality", tolower(label)))
    ) %>%
    filter(!grepl("likely|trust|working|anything r likes", tolower(label))) %>%
    filter(!grepl("where r grew up|tenure|comments|smok", tolower(label))) %>%
    filter(!grepl("how confident|vaccines cause autism", tolower(label))) %>%
    filter(!grepl("checkup and vacc|sleep|knowledge intro", tolower(label))) %>%
    filter(!grepl("(r) sex|sexual orientat|party to deal ", tolower(label))) %>%
    filter(!grepl("alternate nam|name ever chang|cell pho", tolower(label))) %>%
    filter(!grepl("registration|register| order |intervie", tolower(label))) %>%
    filter(!grepl("current unemployment|will affect r's l", tolower(label))) %>%
    filter(!grepl("religion |larger meaning|revised", tolower(label))) %>%
    ## Attitude questions excluded from "issues"
    filter(!grepl("attitude|more or less unemployment", tolower(label))) %>%
    filter(!grepl("r consider|last week|domestic|diploma", tolower(label))) %>%
    filter(!grepl("look for work|religious identification", tolower(label))) %>%
    filter(!grepl("being a religious|armed forces", tolower(label))) %>%
    filter(!grepl("something much larger than themselves", tolower(label))) %>%
    filter(!grepl("any mexican ancestry|spouse", tolower(label))) %>%
    filter(!grepl("private medical|retiree|union", tolower(label))) %>%
    filter(!grepl("occupation|hispanic summary|latino", tolower(label))) %>%
    filter(!grepl("some have more chance in life", tolower(label))) %>%
    filter(!grepl("work for party or candidate", tolower(label))) %>%
    filter(!grepl("party that is more conservative", tolower(label))) %>%
    filter(!grepl("whom does r prefer|political violence", tolower(label))) %>%
    filter(!grepl("r's employment|economy good or bad", tolower(label))) %>%
    filter(!grepl("ukraine|trump|how worried|12 months", tolower(label))) %>%
    filter(!grepl("actively looking for work|lived at add", tolower(label))) %>%
    filter(!grepl("how happy is r|text", tolower(label))) %>%
    filter(!grepl("make government pay attention", tolower(label))) %>%
    filter(!grepl("contributor individuals|based on sympt", tolower(label))) %>%
    filter(!grepl("V20230", var)) %>%
    .$var,
  ## Appendix materials:
  ## demo. + religion (attendance split into two variables)
  set5 = c("V201435", "V201452", "V201453"),
  ## demo. + south
  set6 = "south",
  ## demo. + symbolic ideology, 3-pt
  set7 = "V201200b"
)

## set8 can just re-use set3
vl$set8 <- vl$set3
save(vl, file = here("data", "anes-tidy", "anes-vl-2020.RData"))
