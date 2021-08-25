source(here::here("R", "ANES_1_data.R"))

# Cumulative dataset split by year =============================================
anes_list <- anes %>%
  select(-Version, -VCF0006, -VCF0006a) %>%
  select(
    -matches(
      anes_labels %>%
        filter(grepl("weight", tolower(label))) %>%
        .$var %>%
        paste0(collapse = "|")
    )
  ) %>%
  ## VCF0004 = Year of study
  group_split(VCF0004, .keep = TRUE) %>%
  `names<-`({
    .
  } %>% map(~ .x$VCF0004[1]) %>% unlist()) %>%
  ## VCF0301 = PID vs. VCF0803 = ideology; different!
  map(~ .x %>% mutate(pid7 = VCF0301, pid3 = VCF0303))

# Presidential vote: one-hot encoding ==========================================
anes_onehot <- anes_list[as.character(seq(1952, 2016, by = 4))] %>%
  imap(
    ~ {
      if (.y == 1952) {
        # Somehow, perfectly predicts vote choice
        # Registered to Vote Post-Election
        # Coding error of ANES old data?
        .x %>% select(-VCF0737)
      } else {
        .x
      }
    }
  ) %>%
  ## ANES does not have prez vote if not midterm year
  map(
    ~ data_routine(
      ## Delete almost-identical variables
      .x %>%
        zap_labels() %>%
        ## Because otherwise missing variables will be coded 999
        ## 2,340 out of 59,944 cases
        filter(!is.na(VCF0704a)) %>%
        select(
          -VCF0704, -VCF0705, -VCF0706, ## Prez vote (why so many vars?)
          -VCF0734, ## Intended prez vote vs. actual prez vote
          -VCF0709, ## ticket splitting between Prez and House
          -VCF0710, ## ticket splitting between Prez and Senate
          -VCF0713, ## Pre-survey vote intention
          -VCF0450, ## Approval rating
          -VCF0451, ## Another approval rating (why?!)
          -VCF0302, ## Redundant party ID (either 3-pt or just redundant)
          -VCF0303 ## Redundant party ID (either 3-pt or just redundant)
        ),
      dep = "VCF0704a",
      lvl = c(1, 2),
      lbl = c("DemCand", "RepCand"),
      dbl = c("VCF0101", "pid3", "pid7") ## age
    ) %>%
      train_name_clean()
  )
save(anes_onehot, file = here("data", "anes-tidy", "anes_prezvote.RData"))

# In ANES, there is no equivalent continuous approval; only categorical

# House vote: one-hot encoding =================================================
anes_onehot <- anes_list %>%
  keep(~ sum(!is.na(.x$VCF0707)) > 0) %>%
  map(
    ~ data_routine(
      .x %>%
        zap_labels() %>%
        filter(!is.na(VCF0707)) %>%
        select(
          -VCF0709, ## ticket splitting between Prez and House
          -VCF0302, ## Redundant party ID (either 3-pt or just redundant)
          -VCF0303, ## Redundant party ID (either 3-pt or just redundant)
          -VCF0735, ## Vote for U.S. House- Candidate Code
          -VCF0736, ## Vote for U.S. House- Party
          -VCF1011, ## Respondent Vote for U.S. House Candidate From Own Party
          ## in district of IW
          -VCF0906, ## Thermometer - Democratic House Candidate
          -VCF0907, ## Thermometer - Republican House Candidate
          -VCF0908, ## Thermometer - Republican House Candidate
          -VCF0909, ## Thermometer - Challenger House Candidate
          -VCF9069 ## Strength Approve/Disapprove Running U.S. House Incumbent
        ),
      dep = "VCF0707",
      lvl = c(1, 2),
      lbl = c("DemCand", "RepCand"),
      dbl = c("VCF0101", "pid3", "pid7") ## age
    ) %>%
      train_name_clean()
  )
save(anes_onehot, file = here("data", "anes-tidy", "anes_house.RData"))

# Senate vote: one-hot encoding ================================================
anes_onehot <- anes_list %>%
  keep(~ sum(!is.na(.x$VCF0708)) > 0) %>%
  map(
    ~ data_routine(
      .x %>%
        zap_labels() %>%
        filter(!is.na(VCF0708)) %>%
        select(
          -VCF0710, ## ticket splitting between Prez and Senate
          -VCF0302, ## Redundant party ID (either 3-pt or just redundant)
          -VCF0303, ## Redundant party ID (either 3-pt or just redundant)
          -VCF9056, ## Thermometer - Senate Democratic Candidate
          -VCF9057, ## Thermometer - Senate Republican Candidate
          -VCF9058, ## Thermometer - Senate Incumbent Candidate
          -VCF9059, ## Thermometer - Senate Challenger
          -VCF9060, ## Thermometer - Senator in State with Senate Race
          -VCF9061, ## Thermometer - Senator 1 in State with No Senate Race)
          -VCF9062 ## Thermometer - Senator 2 in State with No Senate Race)
        ),
      dep = "VCF0708",
      lvl = c(1, 2),
      lbl = c("DemCand", "RepCand"),
      dbl = c("VCF0101", "pid3", "pid7") ## age
    ) %>%
      train_name_clean()
  )
save(anes_onehot, file = here("data", "anes-tidy", "anes_senate.RData"))
