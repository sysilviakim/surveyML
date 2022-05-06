library(tidyverse)
library(haven)
library(broom)
library(here)

# Import data
anes <- read_dta(here("data/anes/anes_timeseries_cdf_stata_20211118.dta"))

districtDensity <- readr::read_csv("https://raw.githubusercontent.com/theatlantic/citylab-data/master/citylab-congress/citylab_cdi.csv") %>%
  rename(district_density = Cluster)

anesRECODE <- anes %>%
  filter(VCF0004 >= 2010) %>% # We'll work with post-2010 congressional districts
  transmute(
    year = VCF0004,
    college_grad = ifelse(VCF0110==4,1,0),
    votedRepublican2P = ifelse(VCF0704==2,1,
                               ifelse(VCF0704==1,0,NA)),
    votedDemocrat2P = ifelse(VCF0704==1,1,
                             ifelse(VCF0704==2,0,NA)),
    Republican = ifelse(VCF0301 %in% c(5:7),1,
                        ifelse(is.na(VCF0301),NA,0)),
    Democrat = ifelse(VCF0301 %in% c(1:3), 1,
                      ifelse(is.na(VCF0301),NA,0)),
    female = ifelse(VCF0104==2,1,0),
    white = ifelse(VCF0105a==1,1,0),
    black = ifelse(VCF0105a==2,1,0),
    income = VCF0114,
    age_group = VCF0102,
    district = VCF0900c,  # sadly 2020 values are missing!
    state = VCF0901b,
    distnum = as.character(VCF0900)
  )

anesRECODE <- anesRECODE %>%
  mutate(temp = case_when(
    distnum=="1" ~ "01",
    distnum=="2" ~ "02",
    distnum=="3" ~ "03",
    distnum=="4" ~ "04",
    distnum=="5" ~ "05",
    distnum=="6" ~ "06",
    distnum=="7" ~ "07",
    distnum=="8" ~ "08",
    distnum=="9" ~ "09"
  ),
  #CD = paste(stae,"-",substr(district,3,4),sep = "")
  mutate(CD = paste(substr(district,1,2),"-",substr(district,3,4),sep = ""))
  )


anesRECODE <- left_join(anesRECODE,districtDensity)


voteRepublican <- anesRECODE %>% 
  filter(!is.na(votedRepublican2P)) %>%
  filter(!is.na(district_density)) %>%
  group_by(year) %>%
  do(tidy(lm(votedRepublican2P ~
               factor(district_density) +
               white + black +    
               college_grad +
               female + 
               factor(income) +
               factor(age_group),
             data=. ))) 

voteRepublican   %>% 
  filter(grepl('district',term)) %>%
  mutate(dist = substr(term,25,45)) %>%
  ggplot(aes(x=year,y=estimate,shape = dist)) +
  geom_point(size=2.3) +
  geom_errorbar(aes(ymax = estimate + 1.96*std.error, 
                    ymin = estimate - 1.96*std.error),width=.3) +
  facet_wrap(~dist) +
  labs(y="Estimated marginal effect of place", x = "Year",
       subtitle = "Predicting Republican two-party vote choice") +
  geom_hline(yintercept=0) +
  scale_color_manual(values = c("darkblue","darkred","darkorange")) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=13)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
