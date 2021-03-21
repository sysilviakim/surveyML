# Validating Files =============================================================
source(here::here("R", "utilities.R"))

## Data was downloaded as-is from https://cces.gov.harvard.edu/
## Except for CCES 2017 Data/Guide, which had to be renamed to prevent overwrite
## Downloaded as RData whenever possible as well, but tab file is more accurate
## See table(x$birthyr) for 2016

## File Names ==================================================================
file_ls <- list.files(
  path = here("data/cces"), pattern = ".tab|.RData|.sav|.dta"
) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  set_names(c("file_name")) %>%
  mutate(
    year = parse_number(file_name),
    year = ifelse(nchar(year) == 2, as.numeric(paste0("20", year)), year),
    year = ifelse(nchar(year) == 1, as.numeric(paste0("200", year)), year),
    year = ifelse(file_name == "Common Content Data.RData", 2013, year),
    file_format = tolower(str_extract(file_name, "\\.(.*?)$")),
    file_format = factor(
      file_format, levels = c(".tab", ".rdata", ".dta", ".sav")
    )
  ) %>%
  arrange(desc(year), file_format) %>%
  ## No panel files
  filter(!grepl("panel|county", tolower(file_name))) %>%
  group_by(year) %>%
  slice(1) %>%
  ungroup()

## All are common content? 2007 and 2009 not specified exactly, so nvm...
## assert_that(all(grepl("common", tolower(file_ls$file_name))))
## Any missing years?
assert_that(length(setdiff(seq(2006, 2018), file_ls$year)) == 0)

## Data Import =================================================================
df_ls <- seq(nrow(file_ls)) %>%
  set_names(., paste0("yr_", seq(2006, 2018))) %>%
  map(
    ~ {
      if (file_ls$file_format[.x] == ".tab") {
        read.table(
          here("data/cces", file_ls$file_name[.x]), header = TRUE, 
          fill = TRUE, quote = "", sep = "\t", stringsAsFactors = FALSE
          ## In hindsight, would have better specified colClasses
        )
      } else if (file_ls$file_format[.x] == ".rdata") {
        loadRData(here("data/cces", file_ls$file_name[.x]))
      } else if (file_ls$file_format[.x] == ".dta") {
        ## read.dta(here("data/cces", file_ls$file_name[.x]))
        ## Changing things to make things easier
        read_dta(here("data/cces", file_ls$file_name[.x])) %>%
          zap_labels()
      } else {
        read_spss(here("data/cces", file_ls$file_name[.x]))
      }
    }
  )
