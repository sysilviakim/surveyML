library(tidyverse)
library(pollster)
library(knitr)
library(kableExtra)

loadRData <- function(file_name) {
  load(file_name)
  get(ls()[ls() != "file_name"])
}

CCES2008 <- loadRData("../data/raw/cces_2008_common.RData")

CCES2008$white <- ifelse(CCES2008$V211=="White","White voters","Voters of color")

topline(df = CCES2008, CC327, weight = V201)

topline(df = CCES2008 %>%
          filter(vote_gen08=="validated record of voting in general election"),  # validated voter
         CC327, weight = V201)

crosstab(df = CCES2008 %>%
          filter(vote_gen08=="validated record of voting in general election"),  # validated voter
        y=CC327, 
        x=V213,
        weight = V201)

crosstab_3way(df = CCES2008 %>%
                filter(vote_gen08=="validated record of voting in general election"),  # validated voter
              y=CC327, 
              x=V213,
              z=white,
              weight = V201) %>%
  select(1:4) %>%
  kable("latex",digits=1, booktabs=T) %>%
 # write("../output/tables/cces2008.tex")
  save_kable("../tab/crosstab_cces_2008.pdf",keep_tex = TRUE)

  

