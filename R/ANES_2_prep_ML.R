source("R/ANES_1_data.R")

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
  `names<-`({.} %>% map(~ .x$VCF0004[1]) %>% unlist())

# Routine one-hot encoding =====================================================
anes_onehot <- anes_list[as.character(seq(1948, 2016, by = 4))] %>%
  ## ANES does not have prez vote if not midterm year
  map(
    ~ data_routine(
      ## Delete almost-identical variables
      .x %>% 
        zap_labels() %>%
        select(
          -VCF0704, -VCF0705, -VCF0706, ## Prez vote (why so many vars?)
          -VCF0734, ## Intended prez vote vs. actual prez vote
          -VCF0709, ## ticket splitting between Prez and House
          -VCF0710, ## ticket splitting between Prez and Senate
          -VCF0713, ## Pre-survey vote intention
          -VCF0450, ## Approval rating
          -VCF0451, ## Another approval rating (why?!)
          -VCF0302, ## Redundant party ID (either 3-pt or just redundant)
          -VCF0303  ## Redundant party ID (either 3-pt or just redundant)
        ), 
      dep = "VCF0704a", 
      lvl = c(1, 2), 
      lbl = c("DemCand", "RepCand"),
      dbl = NULL
    ) %>%
      train_name_clean()
  )
## Validate: anes_onehot %>% map("train") %>% map(names) %>% map(1) %>% unlist()
## anes_onehot %>% map("train") %>% map(nrow) %>% unlist()
## anes_onehot %>% map("train") %>% map(ncol) %>% unlist()
save(anes, file = file.path("data", "anes-tidy", "anes_prezvote_full.RData"))
save(
  anes_onehot, 
  file = file.path("data", "anes-tidy", "anes_prezvote_onehot.RData")
)
