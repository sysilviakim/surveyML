source("R/CCES_0_utilities.R")
load(file.path("data", "anes-tidy", "anes_prezvote_onehot.RData"))

## Extract performance measures ================================================
## loop to enable gc()
perf <- list()
vid <- list()

## It turns out that 7-pt party ID is missing for 1948
## See table(anes$VCF0301, anes$VCF0004)
## So exclude 1948
for (yr in as.character(seq(1952, 2016, by = 4))) {
  sfx <- "prezvote"
  temp <- anes_onehot[[as.character(yr)]]

  ## caret results
  for (method in c("cart", "rf")) { ## "lasso", "logit", 
    for (varset in seq(4)) {
      ## Load previously run results
      load(
        file.path(
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
        file.path(
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
      save(vid,  file = "output/ANES/ANES_varimp.RData")
    }
  }
}

# Error in eval(predvars, data, env) : year 2010 sfx 2008vote
# object 'v403_did_not_complete_wave_2' not found
# In addition: There were 16 warnings (use warnings() to see them)
# 16: In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  ...:
# prediction from a rank-deficient fit may be misleading

## Pres Vote Choice, Random Forest =============================================
tab <- seq(4) %>%
  map(~ perf_summ(perf, 1, "rf", .x, yr = seq(1952, 2016, 4))) %>%
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
  xtable(
    label = "tab:ANES_preschoice_rf",
    caption = paste0(
      "Performance Metrics, ", "Presidential Vote Choice, ",
      "Random Forests, ", "ANES 1952--2016"
    ),
    digits = c(0, 0, 0, 4, 4, 4, 4, 4, 4)
  )

print(
  tab, include.rownames = FALSE,
  file = "tab/ANES_prezchoice_rf.tex",
  booktabs = TRUE
)

