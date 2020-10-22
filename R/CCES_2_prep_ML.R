source("R/CCES_1_data.R")
## In presidential election years, delete preference/early vote covariates
## In midterm years, use last presidential vote result as outcome variable

# Take out variables not used in ML ============================================
## Mostly dealt within data_routine automatically, but not always...
df_ls <- df_ls %>%
  map(
    ~ .x %>%
      select(-contains("weight|starttime|endtime")) %>%
      ## I'll keep lookupzip
      select(
        -any_of(c(
          "caseid", "tookpost", "CCEStake", "starttime", "endtime", 
          "DMA", "dmaname", "CL_matched",
          "comptype", "add_confirm", "OScode", "OScode_post", "sample_group",
          "ccesmodule", "CompRating", "CompRating_post", 
          "V100", "V101", "V103", "V200", "V201",
          "v100", "v101", "v103", "v200", "v201"
        ))
      )
  )

# Year 2018 ====================================================================
var_db <- 
  c("birthyr", "citylength_1", "CC18_417c", "child18num", "numchildren")
temp <- data_routine(
  df = df_ls$yr_2018 %>% 
    ## Leave out Trump job approval ratings, both pre and post
    select(-CC18_308a, -CC18_app_dtrmp_post) %>% 
    filter(!is.na(CC18_317) & CC18_317 < 3), 
  dep = "CC18_317", lvl = c(1, 2), lbl = c("trump", "clinton"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2018_2016vote.RData")
temp <- data_routine(
  df = df_ls$yr_2018 %>%
    ## Post-wave Trump job approval ratings CC18_app_dtrmp_post
    ## 2016 vote choice                     CC18_317
    select(-CC18_app_dtrmp_post, -CC18_317) %>%
    filter(!is.na(CC18_308a) & CC18_308a < 5) %>%
    mutate(approv = case_when(CC18_308a < 3 ~ 1, CC18_308a > 2 ~ 0)) %>%
    select(-CC18_308a), 
  dep = "approv", lvl = c(0, 1), lbl = c("dis", "approve"),
  dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2018_trumpapproval.RData")
## Contextual variables consist of the names and parties of the candidates 
## for U.S. House, U.S. Senate, and Governor. For all offices, 
## Candidate 1 is the Democrat and Candidate 2 is the Republican, 
## except when no Democrat is running. When no Democrat is running, the
## Republican is listed as Candidate 1. When only one candidate is running,
## Candidate 2 is listed as "NA".
temp <- data_routine(
  df = df_ls$yr_2018 %>%
    select(-CC18_353, -CC18_353a, -CC18_353x) %>%
    filter(!is.na(CC18_412)) %>%
    mutate(
      house_vote = case_when(
        CC18_412 == 1  & HouseCand1Party == "\"Democratic\"" ~ 1,
        CC18_412 == 2  & HouseCand2Party == "\"Democratic\"" ~ 1,
        CC18_412 == 3  & HouseCand3Party == "\"Democratic\"" ~ 1,
        CC18_412 == 4  & HouseCand4Party == "\"Democratic\"" ~ 1,
        CC18_412 == 10 & HouseCand5Party == "\"Democratic\"" ~ 1,
        CC18_412 == 11 & HouseCand6Party == "\"Democratic\"" ~ 1,
        CC18_412 == 12 & HouseCand7Party == "\"Democratic\"" ~ 1,
        CC18_412 == 1  & HouseCand1Party == "\"Republican\"" ~ 0,
        CC18_412 == 2  & HouseCand2Party == "\"Republican\"" ~ 0,
        CC18_412 == 3  & HouseCand3Party == "\"Republican\"" ~ 0,
        CC18_412 == 4  & HouseCand4Party == "\"Republican\"" ~ 0,
        CC18_412 == 10 & HouseCand5Party == "\"Republican\"" ~ 0,
        CC18_412 == 11 & HouseCand6Party == "\"Republican\"" ~ 0,
        CC18_412 == 12 & HouseCand7Party == "\"Republican\"" ~ 0
      )
    ) %>% 
    select(-CC18_412), 
  dep = "house_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2018_house.RData")
temp <- data_routine(
  df = df_ls$yr_2018 %>%
    ## In the race for U.S. Senator in your state, who do you prefer? etc etc
    select(-CC18_351, -CC18_351a, -CC18_351b, -CC18_351c, -CC18_351x) %>%
    filter(!is.na(CC18_410b) & CC18_410b < 4) %>%
    mutate(
      senate_vote = case_when(
        CC18_410b == 1  & SenCand1Party == "\"Democratic\"" ~ 1,
        CC18_410b == 2  & SenCand2Party == "\"Democratic\"" ~ 1,
        CC18_410b == 3  & SenCand3Party == "\"Democratic\"" ~ 1,
        CC18_410b == 1  & SenCand1Party == "\"Republican\"" ~ 0,
        CC18_410b == 2  & SenCand2Party == "\"Republican\"" ~ 0,
        CC18_410b == 3  & SenCand3Party == "\"Republican\"" ~ 0
      )
    ) %>% 
    select(-CC18_410b), 
  dep = "senate_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2018_senate.RData")

# Year 2016 ====================================================================
var_db <- c(
  "birthyr", "citylength_1", "CC16_415r", "CC16_416r", 
  "CC16_417c", "child18num"
)
temp <- data_routine(
  df = df_ls$yr_2016 %>% 
    ## Early Vote 2016                             CC16_364b
    ## Which candidate do you prefer for president CC16_364c
    ## 2012 Presidential Vote Choice               CC16_326
    ## 2016 Primary Vote Choice                    CC16_328
    ## Vote choice for other levels of government are still there
    ## Approval rating
    select(-CC16_364b, -CC16_364c, -CC16_326, -CC16_328, -CC16_320a) %>%
    filter(!is.na(CC16_410a) & CC16_410a < 3), 
  dep = "CC16_410a", lvl = c(1, 2), lbl = c("trump", "clinton"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2016_2016vote.RData")
temp <- data_routine(
  df = df_ls$yr_2016 %>%
    ## Same as above + presidential vote choice
    select(-CC16_364b, -CC16_364c, -CC16_326, -CC16_328, -CC16_410a) %>%
    filter(!is.na(CC16_320a) & CC16_320a < 5) %>%
    mutate(approv = case_when(CC16_320a < 3 ~ 1, CC16_320a > 2 ~ 0)) %>%
    select(-CC16_320a), 
  dep = "approv", lvl = c(0, 1), lbl = c("dis", "approve"),
  dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2016_obamaapproval.RData")
## In 2016, CC16_412 and CC16_410b are not listed explicitly, so guesswork
temp <- data_routine(
  df = df_ls$yr_2016 %>%
    select(-CC16_367, -CC16_367a, -CC16_367x) %>%
    filter(!is.na(CC16_412)) %>%
    mutate(
      house_vote = case_when(
        CC16_412 == 1  & HouseCand1Party  == "\"Democratic\"" ~ 1,
        CC16_412 == 2  & HouseCand2Party  == "\"Democratic\"" ~ 1,
        CC16_412 == 3  & HouseCand3Party  == "\"Democratic\"" ~ 1,
        CC16_412 == 4  & HouseCand4Party  == "\"Democratic\"" ~ 1,
        CC16_412 == 10 & HouseCand5Party  == "\"Democratic\"" ~ 1,
        CC16_412 == 11 & HouseCand6Party  == "\"Democratic\"" ~ 1,
        CC16_412 == 12 & HouseCand7Party  == "\"Democratic\"" ~ 1,
        CC16_412 == 13 & HouseCand8Party  == "\"Democratic\"" ~ 1,
        CC16_412 == 14 & HouseCand9Party  == "\"Democratic\"" ~ 1,
        CC16_412 == 15 & HouseCand10Party == "\"Democratic\"" ~ 1,
        CC16_412 == 16 & HouseCand11Party == "\"Democratic\"" ~ 1,
        CC16_412 == 1  & HouseCand1Party  == "\"Republican\"" ~ 0,
        CC16_412 == 2  & HouseCand2Party  == "\"Republican\"" ~ 0,
        CC16_412 == 3  & HouseCand3Party  == "\"Republican\"" ~ 0,
        CC16_412 == 4  & HouseCand4Party  == "\"Republican\"" ~ 0,
        CC16_412 == 10 & HouseCand5Party  == "\"Republican\"" ~ 0,
        CC16_412 == 11 & HouseCand6Party  == "\"Republican\"" ~ 0,
        CC16_412 == 12 & HouseCand7Party  == "\"Republican\"" ~ 0,
        CC16_412 == 13 & HouseCand8Party  == "\"Republican\"" ~ 0,
        CC16_412 == 14 & HouseCand9Party  == "\"Republican\"" ~ 0,
        CC16_412 == 15 & HouseCand10Party == "\"Republican\"" ~ 0,
        CC16_412 == 16 & HouseCand11Party == "\"Republican\"" ~ 0
      )
    ) %>% select(-CC16_412), 
  dep = "house_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2016_house.RData")
temp <- data_routine(
  df = df_ls$yr_2016 %>%
    select(-CC16_365, -CC16_365a, -CC16_365x) %>%
    filter(!is.na(CC16_410b) & CC16_410b < 4) %>%
    mutate(
      senate_vote = case_when(
        CC16_410b == 1  & SenCand1Party == "\"Democratic\"" ~ 1,
        CC16_410b == 2  & SenCand2Party == "\"Democratic\"" ~ 1,
        CC16_410b == 3  & SenCand3Party == "\"Democratic\"" ~ 1,
        CC16_410b == 1  & SenCand1Party == "\"Republican\"" ~ 0,
        CC16_410b == 2  & SenCand2Party == "\"Republican\"" ~ 0,
        CC16_410b == 3  & SenCand3Party == "\"Republican\"" ~ 0
      )
    ) %>% select(-CC16_410b), 
  dep = "senate_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2016_senate.RData")

# Year 2014 ====================================================================
var_db <- c("birthyr", "CC417c", "child18num")
temp <- data_routine(
  df = df_ls$yr_2014 %>% 
    ## Leave out Obama approval rate
    select(-CC14_308a) %>%
    filter(!is.na(CC14_317) & CC14_317 < 3), 
  dep = "CC14_317", lvl = c(2, 1), lbl = c("romney", "obama"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2014_2012vote.RData")
temp <- data_routine(
  df = df_ls$yr_2014 %>%
    ## Leave out 2012 vote choice
    select(-CC14_317) %>%
    filter(!is.na(CC14_308a) & CC14_308a < 5) %>%
    mutate(approv = case_when(CC14_308a < 3 ~ 1, CC14_308a > 2 ~ 0)) %>%
    select(-CC14_308a), 
  dep = "approv", lvl = c(0, 1), lbl = c("dis", "approve"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2014_obamaapproval.RData")
temp <- data_routine(
  df = df_ls$yr_2014 %>%
    select(-CC360, -CC360x) %>%
    filter(!is.na(CC412)) %>%
    mutate(
      house_vote = case_when(
        CC412 == 1  & HouseCand1Party  == "\"Democratic\"" ~ 1,
        CC412 == 2  & HouseCand2Party  == "\"Democratic\"" ~ 1,
        CC412 == 3  & HouseCand3Party  == "\"Democratic\"" ~ 1,
        CC412 == 1  & HouseCand1Party  == "\"Republican\"" ~ 0,
        CC412 == 2  & HouseCand2Party  == "\"Republican\"" ~ 0,
        CC412 == 3  & HouseCand3Party  == "\"Republican\"" ~ 0
      )
    ) %>% select(-CC412), 
  dep = "house_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2014_house.RData")
temp <- data_routine(
  df = df_ls$yr_2014 %>%
    select(-CC355, -CC355x) %>%
    filter(!is.na(CC410b) & CC410b < 4) %>%
    mutate(
      senate_vote = case_when(
        CC410b == 1  & SenCand1Party == "\"Democratic\"" ~ 1,
        CC410b == 2  & SenCand2Party == "\"Democratic\"" ~ 1,
        CC410b == 3  & SenCand3Party == "\"Democratic\"" ~ 1,
        CC410b == 1  & SenCand1Party == "\"Republican\"" ~ 0,
        CC410b == 2  & SenCand2Party == "\"Republican\"" ~ 0,
        CC410b == 3  & SenCand3Party == "\"Republican\"" ~ 0
      )
    ) %>% select(-CC410b), 
  dep = "senate_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2014_senate.RData")

# Year 2012 ====================================================================
var_db <- c("birthyr", "child18numx")
temp <- data_routine(
  df = df_ls$yr_2012 %>%
    ## Early Vote 2012                             CC354b
    ## Which candidate do you prefer for president CC354c
    ## 2008 Presidential Vote Choice               CC317
    ## (no primary vote question)
    ## Vote choice for other levels of government are still there
    ## Approval rating                             CC308a
    select(-CC354b, -CC354c, -CC317, -CC308a) %>%
    filter(!is.na(CC410a) & CC410a < 3), 
  dep = "CC410a", lvl = c(2, 1), lbl = c("mccain", "obama"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2012_2012vote.RData")
temp <- data_routine(
  df = df_ls$yr_2012 %>%
    ## Early Vote 2012                             CC354b
    ## Which candidate do you prefer for president CC354c
    ## 2008 Presidential Vote Choice               CC317
    ## (no primary vote question)
    ## Vote choice for other levels of government are still there
    ## Presidential Vote Choice
    select(-CC354b, -CC354c, -CC317, -CC410a) %>%
    filter(!is.na(CC308a) & CC308a < 5) %>%
    mutate(approv = case_when(CC308a < 3 ~ 1, CC308a > 2 ~ 0)) %>%
    select(-CC308a), 
  dep = "approv", lvl = c(0, 1), lbl = c("dis", "approve"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2012_obamaapproval.RData")
temp <- data_routine(
  df = df_ls$yr_2012 %>%
    select(-CC390, -CC390b) %>%
    filter(!is.na(CC412)) %>%
    mutate(
      house_vote = case_when(
        CC412 == 1  & HouseCand1Party  == "\"Democratic\"" ~ 1,
        CC412 == 2  & HouseCand2Party  == "\"Democratic\"" ~ 1,
        CC412 == 3  & HouseCand3Party  == "\"Democratic\"" ~ 1,
        CC412 == 1  & HouseCand1Party  == "\"Republican\"" ~ 0,
        CC412 == 2  & HouseCand2Party  == "\"Republican\"" ~ 0,
        CC412 == 3  & HouseCand3Party  == "\"Republican\"" ~ 0
      )
    ) %>% select(-CC412), 
  dep = "house_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2012_house.RData")
temp <- data_routine(
  df = df_ls$yr_2012 %>%
    select(-CC355, -CC355b) %>%
    filter(!is.na(CC410b) & CC410b < 4) %>%
    mutate(
      senate_vote = case_when(
        CC410b == 1  & SenCand1Party == "\"Democratic\"" ~ 1,
        CC410b == 2  & SenCand2Party == "\"Democratic\"" ~ 1,
        CC410b == 3  & SenCand3Party == "\"Democratic\"" ~ 1,
        CC410b == 1  & SenCand1Party == "\"Republican\"" ~ 0,
        CC410b == 2  & SenCand2Party == "\"Republican\"" ~ 0,
        CC410b == 3  & SenCand3Party == "\"Republican\"" ~ 0
      )
    ) %>% select(-CC410b), 
  dep = "senate_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2012_senate.RData")

# Year 2010 ====================================================================
## This was a Stata export so lots of factors, unfortunately 
## 2010 needs additional name cleaning step
var_db <- c("V207", "CC417c") ## V207 is birth year, 417c is donations
df <- df_ls$yr_2010 %>%
  mutate_if(is.POSIXt, as.Date) %>%
  ## Interview start time        V401
  ## Interview end time          V402
  ## Interview start time (post) V404
  ## Interview end time   (post) V405
  ## Obama approval              CC308a
  select(-V401, -V402, -V404, -V405) %>%
  mutate_if(is.factor, fct_explicit_na, na_level = "999")
temp <- data_routine(
  df = df, dep = "CC317", dbl = var_db,
  lvl = c(2, 1), lbl = c("mccain", "obama")
)
save(temp, file = "data/cces-tidy/data_2010_2008vote.RData")
temp <- data_routine(
  df = df %>%
    ## Vote choice 2010 CC317
    select(-CC317) %>%
    mutate(
      approv = case_when(CC308a %in% c(1, 2) ~ 1, CC308a %in% c(3, 4) ~ 0)
    ) %>%
    select(-CC308a), 
  dep = "approv", lvl = c(0, 1), lbl = c("dis", "approve"), dbl = var_db
)
temp <- train_name_clean(temp)
save(temp, file = "data/cces-tidy/data_2010_obamaapproval.RData")
temp <- data_routine(
  df = df %>%
    select(-CC390, -CC390a) %>%
    mutate(house_vote = case_when(CC412 == 1 ~ 1, CC412 == 2 ~ 0)) %>%
    select(-CC412), 
  dep = "house_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
temp <- train_name_clean(temp)
save(temp, file = "data/cces-tidy/data_2010_house.RData")
temp <- data_routine(
  df = df %>%
    select(-CC355, -CC355a, -CC355b) %>%
    mutate(senate_vote = case_when(CC410b == 1 ~ 1, CC410b == 2 ~ 0)) %>%
    select(-CC410b), 
  dep = "senate_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
temp <- train_name_clean(temp)
save(temp, file = "data/cces-tidy/data_2010_senate.RData")

# Year 2008 ====================================================================
var_db <- c("V207", "CC416b") ## V207 is birth year, 417c is donations
temp <- data_routine(
  df = df_ls$yr_2008 %>% 
    ## Early vote                                  CC326b
    ## Vote intention (pre-wave)                   CC327
    ## (no which candidate do you prefer for president) 
    ## (no 2004 vote choice question)
    ## Primary vote                                CC325a
    ## Caucus Vote                                 CC325b
    ## Vote choice for other levels of government are still there
    ## Bush approval                               CC335bush
    select(-CC326b, -CC327, -CC325a, -CC325b, -CC335bush) %>%
    filter(!is.na(CC410) & CC410 < 3), 
  dep = "CC410", lvl = c(2, 1), lbl = c("mccain", "obama"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2008_2008vote.RData")
temp <- data_routine(
  df = df_ls$yr_2008 %>%
    ## Early vote                                  CC326b
    ## Vote intention (pre-wave)                   CC327
    ## (no which candidate do you prefer for president) 
    ## (no 2004 vote choice question)
    ## Primary vote                                CC325a
    ## Caucus Vote                                 CC325b
    ## Vote choice for other levels of government are still there
    ## Vote choice president                       CC410
    select(-CC326b, -CC327, -CC325a, -CC325b, -CC410) %>%
    filter(!is.na(CC335bush) & CC335bush < 5) %>%
    mutate(approv = case_when(CC335bush < 3 ~ 1, CC335bush > 2 ~ 0)) %>%
    select(-CC335bush), 
  dep = "approv", lvl = c(0, 1), lbl = c("dis", "approve"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2008_bushapproval.RData")
temp <- data_routine(
  df = df_ls$yr_2008 %>%
    select(-CC339, -CC340) %>%
    filter(!is.na(CC412)) %>%
    mutate(house_vote = case_when(CC412 == 1 ~ 1, CC412 == 2 ~ 0)) %>%
    select(-CC412), 
  dep = "house_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2008_house.RData")
temp <- data_routine(
  df = df_ls$yr_2008 %>%
    select(-CC335, -CC335S2, -CC337, -CC337S2) %>%
    filter(!is.na(CC411) & CC411 < 4) %>%
    mutate(senate_vote = case_when(CC411 == 1 ~ 1, CC411 == 2 ~ 0)) %>%
    select(-CC411), 
  dep = "senate_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2008_senate.RData")

# Year 2006 ====================================================================
var_db <- c(
  "v2020", "v2040", "v2129", paste0("v30", seq(42, 53)), 
  paste0("v40", seq(22, 33)), "v4040", "v4042", "v4063"
) 
df <- df_ls$yr_2006 %>% 
  mutate(
    v2040 = tolower(v2040), 
    v2040 = ifelse(v2040 == "\"one\"",               "1", v2040),
    v2040 = ifelse(v2040 == "\"two\"",               "2", v2040),
    v2040 = ifelse(v2040 == "\"three\"",             "3", v2040),
    v2040 = ifelse(v2040 == "\"four\"",              "4", v2040),
    v2040 = ifelse(v2040 == "\"five\"",              "5", v2040),
    v2040 = ifelse(v2040 == "\"seven\"",             "7", v2040),
    v2040 = ifelse(v2040 == "\"nine years\"",        "9", v2040),
    v2040 = ifelse(v2040 == "\"ten\"",              "10", v2040),
    v2040 = ifelse(v2040 == "\"same ten years!\"",  "10", v2040),
    v2040 = ifelse(v2040 == "\"twelve\"",           "12", v2040),
    v2040 = parse_number(v2040),
    v2040 = ifelse(v2040 > 60 | v2040 < 0, NA, v2040),
    v4063 = gsub("o", "0", v4063),
    v4063 = parse_number(v4063)
  )
## Does not seem to have 2004 vote
## Bush approval page 32---variable does not seem to be there
temp <- data_routine(
  df = df %>%
    select(-v3054) %>%
    filter(!is.na(v4015) & v4015 < 3) %>%
    mutate(house_vote = case_when(v4015 == 1 ~ 1, v4015 == 2 ~ 0)) %>%
    select(-v4015), 
  dep = "house_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2006_house.RData")
temp <- data_routine(
  df = df %>%
    select(-v3056) %>%
    filter(!is.na(v4014) & v4014 < 3) %>%
    mutate(senate_vote = case_when(v4014 == 1 ~ 1, v4014 == 2 ~ 0)) %>%
    select(-v4014), 
  dep = "senate_vote", lvl = c(0, 1), lbl = c("rep", "dem"), dbl = var_db
)
save(temp, file = "data/cces-tidy/data_2006_senate.RData")