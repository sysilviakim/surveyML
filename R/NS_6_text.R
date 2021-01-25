# Extract information from the Nationscape dataset
# for the text of the paper

source("R/NS_extract_performance.R")

# Average number of responses per wave:
a_release2 %>% count(week) %>% summarise(avg = mean(n))

