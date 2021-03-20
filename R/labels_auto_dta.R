source(here::here("R", "labels_handcoded.R"))

# CCES: file names =============================================================
file_ls <- list.files(path = "data/cces", pattern = ".tab|.RData|.sav|.dta") %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  set_names(c("file_name")) %>%
  mutate(
    year = parse_number(file_name),
    year = ifelse(nchar(year) == 2, as.numeric(paste0("20", year)), year),
    year = ifelse(nchar(year) == 1, as.numeric(paste0("200", year)), year),
    year = ifelse(file_name == "Common Content Data.RData", 2013, year),
    file_format = tolower(str_extract(file_name, "\\.(.*?)$")),
    file_format = factor(
      file_format,
      levels = c(".dta", ".sav", ".rdata", ".tab")
    )
  ) %>%
  arrange(desc(year), file_format) %>%
  ## No panel files
  filter(!grepl("panel|county|cumulative", tolower(file_name))) %>%
  group_by(year) %>%
  slice(1) %>%
  ungroup() %>%
  filter(year %in% cces_years)

# Only .rdata or .dta ==========================================================
cces <- seq(nrow(file_ls)) %>%
  set_names(., paste0("yr_", cces_years)) %>%
  map(
    ~ {
      if (file_ls$file_format[.x] == ".dta") {
        read_dta(here("data/cces", file_ls$file_name[.x]))
      }
    }
  )

# CCES: 2018 and 2010 only =====================================================
dta_labels <- cces %>%
  keep(~ !is.null(.x)) %>%
  map(~ select(.x, where(is.labelled))) %>%
  map(
    function(x) {
      x %>%
        imap_dfr(
          ~ c(attributes(.x)$labels, "answer missing" = 999) %>%
            enframe() %>%
            mutate(
              name = paste0(attributes(.x)$label, ": ", name),
              value = paste0(.y, ".", value)
            )
        )
    }
  ) %>%
  imap_dfr(~ .x %>% mutate(wave = gsub("yr_20", "var", .y)))

# CCES: rename to short, succinct labels =======================================
temp <- dta_labels %>%
  mutate(
    name = case_when(
      grepl(" point ", name) ~ gsub(" point ", "-pt", name),
      grepl(" -- ", name) ~ gsub(" -- ", ": ", name),
      grepl("Family Income", name) ~ gsub("Family Income", "Income", name),
      TRUE ~ name
    )
  )

# Append to handcoded labels, but handcoded ones take priority =================
for (wave in c("var10", "var18")) {
  vl[[wave]] <- temp %>%
    filter(wave == wave) %>%
    bind_rows(., enframe(vl[[wave]]) %>% rename(value = name, name = value)) %>%
    arrange(value) %>%
    dedup() %>%
    group_by(value) %>%
    # Choose handcoded over autocoded
    slice(n()) %>%
    select(value, name) %>%
    deframe()
}

# ANES: data import ============================================================
anes <- read_dta(here("data/anes/anes_timeseries_cdf.dta")) %>%
  select(where(is.labelled))

vl$anes <- anes %>%
  imap_dfr(
    ~ c(attributes(.x)$labels, "answer missing" = 999) %>%
      enframe() %>%
      mutate(
        name = paste0(attributes(.x)$label, ": ", name),
        value = paste0(.y, "_", value)
      )
  ) %>%
  mutate(value = tolower(value)) %>%
  bind_rows(., enframe(vl$anes) %>% rename(value = name, name = value)) %>%
  arrange(value) %>%
  dedup() %>%
  group_by(value) %>%
  # Choose handcoded over autocoded
  slice(n()) %>%
  select(value, name) %>%
  deframe()

assert_that(length(vl$anes) > 5000)

# Save output ==================================================================
save(vl, file = here("data", "variable_labels.Rda"))
