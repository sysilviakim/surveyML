# Validating Files =============================================================

## Data was downloaded as-is from https://cces.gov.harvard.edu/
## Except for CCES 2017 Data/Guide, which had to be renamed to prevent overwrite
## Downloaded as RData whenever possible

file_ls <- list.files(path = "data/raw", pattern = ".RData|.sav|.dta") %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  set_names(c("file_name")) %>%
  mutate(
    year = parse_number(file_name),
    year = ifelse(nchar(year) == 2, as.numeric(paste0("20", year)), year),
    year = ifelse(nchar(year) == 1, as.numeric(paste0("200", year)), year),
    year = ifelse(file_name == "Common Content Data.RData", 2013, year),
    file_format = tolower(str_extract(file_name, "\\.(.*?)$")),
    file_format = factor(file_format, levels = c(".rdata", ".dta", ".sav"))
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
setdiff(seq(2006, 2018), file_ls$year) 

df_ls <- seq(nrow(file_ls)) %>%
  set_names(., paste0("yr_", seq(2018, 2006))) %>%
  map(
    ~ {
      if (file_ls$file_format[.x] == ".rdata") {
        loadRData(file.path("data/raw", file_ls$file_name[.x]))
      } else if (file_ls$file_format[.x] == ".dta") {
        read.dta(file.path("data/raw", file_ls$file_name[.x]))
      } else {
        read.spss(file.path("data/raw", file_ls$file_name[.x]))
      }
    }
  )

