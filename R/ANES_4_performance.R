source(here::here("R", "utilities.R"))
load(here("data", "variable_labels.Rda"))

if (!dir.exists(here("tab", "perf"))) {
  dir.create(here("tab", "perf"))
}

## Extract performance measures ================================================
vid <- perf <- list()

## It turns out that 7-pt party ID is missing for 1948
## See table(anes$VCF0301, anes$VCF0004)
## So exclude 1948

anes_years <- seq(1952, 2020, by = 4)
for (sfx in c("prezvote")) { ## , "house", "senate"
  load(here("data", "anes-tidy", paste0("anes_", sfx, ".RData")))
  for (yr in as.character(anes_years)) {
    temp <- anes_onehot[[as.character(yr)]]

    ## caret results
    for (method in c("logit", "cart", "rf")) {
      for (varset in seq(8)) {
        ## Load previously run results
        load(
          here(
            "output", "ANES", method,
            paste0(method, "_", yr, "_", sfx, "_st", varset, ".RData")
          )
        )

        ## Evaluate performance
        perf[[paste0("year", yr)]][[sfx]][[method]][[paste0("set", varset)]] <-
          perf_routine(
            method,
            eval(parse(text = paste0("turn.", method))),
            temp$test,
            dv = levels(temp$test$depvar)
          )

        ## Plot variable importance
        pdf_varimp(
          eval(parse(text = paste0("turn.", method))),
          here(
            "fig", "ANES", method,
            paste0("var", method, "_", yr, "_", sfx, "_st", varset, ".pdf")
          ),
          labels = vl[["anes"]],
          font = "CM Roman"
        )

        vid_temp <- varImp(eval(parse(text = paste0("turn.", method))))
        vid[[paste0("year", yr)]][[sfx]][[method]][[paste0("set", varset)]] <-
          vid_temp$importance %>% Kmisc::rowid_matrix_to_df()

        ## Save memory
        rm(list = paste0("turn.", method))
        gc(reset = TRUE)
        save(perf, file = here("output/ANES/ANES_perf.RData"))
        save(vid, file = here("output/ANES/ANES_varimp.RData"))
      }
    }
  }
}

# xtable export ================================================================
load(here("output/ANES/ANES_perf.RData"))
load(here("output/ANES/ANES_varimp.RData"))

cross2(c("prezvote"), c("logit", "cart", "rf")) %>% ## , "house", "senate"
  map(
    ~ {
      tab <- seq(8) %>%
        map(
          function(x) perf_summ(perf, .x[[1]], .x[[2]], x, yr = anes_years)
        ) %>%
        bind_rows(.id = "Set") %>%
        arrange(desc(Year), Set) %>%
        mutate(Set = factor(Set, levels = seq(8), labels = set_labels)) %>%
        rename(`Variable Specification` = Set) %>%
        select(-AUC_lower, -AUC_upper, -Accuracy_lower, -Accuracy_upper) %>%
        xtable(
          label = paste0("tab:ANES_", .x[[1]], "_", .x[[2]]),
          caption = paste0(
            "Performance Metrics, ",
            ifelse(
              .x[[1]] == "prezvote", "Presidential Vote Choice, ",
              ifelse(
                .x[[1]] == "house", "House Vote Choice, ",
                "Senate Vote Choice, "
              )
            ),
            ifelse(
              .x[[2]] == "logit", "Logit, ",
              ifelse(.x[[2]] == "cart", "CART, ", "Random Forests, ")
            ),
            "ANES 1952--2020"
          ),
          digits = c(0, 0, 0, 4, 4, 4, 4, 4, 4)
        )

      print(
        tab,
        include.rownames = FALSE,
        file = here(
          "tab", "perf", paste0("ANES_", .x[[1]], "_", .x[[2]], ".tex")
        ),
        booktabs = TRUE, floating = FALSE, tabular.environment = "longtable"
      )
    }
  )

# Alternative, just accuracy between methods ===================================
for (sfx in c("prezvote")) { ## , "house", "senate"
  tab <- c("logit", "cart", "rf") %>%
    map(
      ~ seq(8) %>%
        map_dfr(
          function(x) {
            perf_summ(perf, sfx, .x, x, yr = anes_years) %>%
              select(Year, !!as.name(.x) := CI) %>%
              mutate(Set = x)
          }
        ) %>%
        select(Set, everything()) %>%
        arrange(desc(Year), Set) %>%
        mutate(Set = factor(Set, levels = seq(8), labels = set_labels)) %>%
        rename(`Variable Specification` = Set)
    ) %>%
    Reduce(left_join, .) %>%
    rename(Logit = logit, CART = cart, RF = rf) %>%
    xtable(
      label = paste0("tab:ANES_", sfx, "_accuracy"),
      caption = paste0(
        "Accuracy Range Comparison, ",
        paste0(
          ifelse(
            sfx == "prezvote", "Presidential",
            ifelse(sfx == "house", "House", "Senate")
          ),
          " Vote Choice, "
        ),
        "ANES 1952--2016"
      ),
      digits = 0
    )
  print(
    tab,
    include.rownames = FALSE,
    file = here("tab", "perf", paste0("ANES_", sfx, "_accuracy.tex")),
    booktabs = TRUE, floating = FALSE, tabular.environment = "longtable"
  )
}
