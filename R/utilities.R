library(dplyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(foreign)
library(assertthat)

loadRData <- function(file_name){
  load(file_name)
  get(ls()[ls() != "file_name"])
}
