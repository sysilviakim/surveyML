source(here::here("R", "ANES_1_data.R"))

# Cumulative dataset split by year =============================================
anes_list <- anes %>%
  ## VCF0004 = Year of study
  group_split(VCF0004, .keep = TRUE) %>%
  `names<-`({.} %>% map(~ .x$VCF0004[1]) %>% unlist()) %>%
  ## VCF0301 = PID vs. VCF0803 = ideology; different!
  map(~ .x %>% mutate(pid7 = VCF0301, pid3 = VCF0303))

# Presidential vote: one-hot encoding ==========================================
anes_onehot <- anes_list[as.character(anes_years)] %>%
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
save(anes_onehot, file = here("data", "anes-tidy", "anes_onehot_prez.Rda"))

anes_onehot_pid <- within(anes_list) %>%
  map(
    ~ data_routine(
      .x %>% zap_labels() %>% mutate(dummy = row_number() %% 2),
      dep = "dummy", ## Otherwise, those who didn't vote for president attrition
      lvl = c(0, 1),
      lbl = c("Even", "Odd"), ## Doesn't mean anything
      dbl = c("VCF0101", "pid3", "pid7")
    ) %>%
      train_name_clean()
  )
save(anes_onehot_pid, file = here("data", "anes-tidy", "anes_onehot_pid.Rda"))
