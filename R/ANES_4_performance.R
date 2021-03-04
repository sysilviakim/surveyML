source(here::here("R", "utilities.R"))
if (!dir.exists(here("tab", "perf"))) {
  dir.create(here("tab", "perf"))
}

## Extract performance measures ================================================
vid <- perf <- list()

## It turns out that 7-pt party ID is missing for 1948
## See table(anes$VCF0301, anes$VCF0004)
## So exclude 1948

for (sfx in c("prezvote", "house", "senate")) {
  load(here("data", "anes-tidy", paste0("anes_", sfx, ".RData")))
  for (yr in as.character(seq(1952, 2016, by = 4))) {
    temp <- anes_onehot[[as.character(yr)]]

    ## caret results
    for (method in c("logit", "cart", "rf")) {
      for (varset in seq(4)) {
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
        save(perf, file = "output/ANES/ANES_perf.RData")
        save(vid, file = "output/ANES/ANES_varimp.RData")
      }
    }
  }
}

# xtable export ================================================================
cross2(c("prezvote", "house", "senate"), c("logit", "cart", "rf")) %>%
  map(
    ~ {
      tab <- seq(4) %>%
        map(
          function(x) {
            perf_summ(
              perf, .x[[1]], .x[[2]], x,
              yr = seq(1952, 2016, 4)
            )
          }
        ) %>%
        bind_rows(.id = "Set") %>%
        arrange(desc(Year), Set) %>%
        mutate(
          Set = factor(
            Set,
            levels = seq(4), labels = c(
              "Demographics", "Demo. + PID", "Demo. + PID + Issues",
              "All Observables"
            )
          )
        ) %>%
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
            "ANES 1952--2016"
          ),
          digits = c(0, 0, 0, 4, 4, 4, 4, 4, 4)
        )

      print(
        tab,
        include.rownames = FALSE,
        file = here(
          "tab", "perf", paste0("ANES_", .x[[1]], "_", .x[[2]], ".tex")
        ),
        booktabs = TRUE,
        floating = FALSE,
        tabular.environment = "longtable"
      )
    }
  )
