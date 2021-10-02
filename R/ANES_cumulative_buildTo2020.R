library(tidyverse)
library(haven)
library(broom)

anes2020_temp <- read_dta("~/Dropbox/B_polls/ANES/2020panel/anes_timeseries_2020_stata_20210324.dta")

# anes2016_cumulative <- read_dta("data/anes/anes_timeseries_cdf.dta")
anes2016_cumulative <- read_dta("~/Dropbox/B_polls/ANES/cumulative_to_2016/anes_timeseries_cdf.dta")


                           

anes2016 <- anes2016_cumulative %>%
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
    age = VCF0102
  )


# Deal with missing values:
anes2020_temp$V201617x[anes2020_temp$V201617x==-9] <- NA
anes2020_temp$V201617x[anes2020_temp$V201617x==-5] <- NA
anes2020_temp$V201507x[anes2020_temp$V201507x==-9] <- NA


anes2020 <- anes2020_temp %>%
  transmute(year=2020,
        votedRepublican2P = ifelse(V202073==2,1,
                                       ifelse(V202073==1,0,NA)),
        votedDemocrat2P = ifelse(V202073==1,1,
                                     ifelse(V202073==2,0,NA)),
        white = ifelse(V201549x==1,1,0),
        black = ifelse(V201549x==2,1,0),
        college_grad = ifelse(V201510 == 6 | V201510 == 7 | V201510 ==8,1,0),
        Democrat = ifelse(V201231x >= 1 & V201231x <=3,1,0),
        female = ifelse(V201600==2,1,0),
        Republican = ifelse(V201231x==5 | V201231x==6 | V201231x==7,1,0),
        income = V201617x,
        age = V201507x
  )


anes_cumulative_to_2020 <- bind_rows(
  anes2016,
  anes2020
)

