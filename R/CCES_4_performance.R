source("R/CCES_0_utilities.R")

## Extract performance measures ================================================
## loop to enable gc()
if (file.exists("output/CCES/CCES_perf.RData")) {
  load("output/CCES/CCES_perf.RData")
  load("output/CCES/CCES_varimp.RData")
} else {
  perf <- list()
  vid <- list()
}

for (yr in rev(seq(2006, 2018, 2))) {
  for (i in seq(length(file_suffix[[paste0("year", yr)]]))) {
    sfx <- file_suffix[[paste0("year", yr)]][i]
    load(
      file.path("data", "cces-tidy", paste0("data_", yr, "_", sfx, ".RData"))
    )

    ## caret results
    for (method in c("cart", "lasso", "logit", "rf")) {
      for (varset in seq(4)) {
        
        ## Load previously run results
        load(
          file.path(
            "output", "CCES", method, 
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
            "fig", "CCES", method,
            paste0("var", method, "_", yr, "_", sfx, "_st", varset, ".pdf")
          ),
          labels = vl[[paste0("var", substr(yr, 3, 4))]],
          font = "CM Roman"
        )
        
        vid_temp <- varImp(eval(parse(text = paste0("turn.", method))))
        vid[[paste0("year", yr)]][[sfx]][[method]][[paste0("set", varset)]] <- 
          vid_temp$importance %>% Kmisc::rowid_matrix_to_df()
        
        ## Save memory
        rm(list = paste0("turn.", method))
        gc(reset = TRUE)
        save(perf, file = "output/CCES/CCES_perf.RData")
        save(vid,  file = "output/CCES/CCES_varimp.RData")
      }
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
  map(~ perf_summ(1, "rf", .x)) %>%
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
  filter(Year != 2006) %>%
  xtable(
    label = "tab:cces_preschoice_rf",
    caption = paste0(
      "Performance Metrics, ", "Presidential Vote Choice, ",
      "Random Forests, ", "CCES 2006--2018"
    ),
    digits = c(0, 0, 0, 4, 4, 4, 4, 4, 4)
  )

print(
  tab, include.rownames = FALSE,
  file = "tab/CCES_preschoice_rf.tex",
  booktabs = TRUE
)

## Pres Approval Ratings, Random Forest ========================================
tab <- seq(4) %>%
  map(~ perf_summ(2, "rf", .x)) %>%
  bind_rows(.id = "Specification") %>%
  arrange(desc(Year), Specification) %>%
  xtable(
    label = "tab:cces_presapprov_rf",
    caption = paste0(
      "Performance Metrics, ", "Presidential Approval Rating, ",
      "Random Forests, ", "CCES 2006--2018"
    ),
    digits = c(0, 0, 0, 4, 4, 4, 4, 4, 4)
  )

print(
  tab, include.rownames = FALSE,
  file = "tab/CCES_presapprov_rf.tex"
)

## House Vote Choice, Random Forest ============================================
tab <- seq(4) %>%
  map(~ perf_summ("house", "rf", .x)) %>%
  bind_rows(.id = "Specification") %>%
  arrange(desc(Year), Specification) %>%
  xtable(
    label = "tab:cces_house_rf",
    caption = paste0(
      "Performance Metrics, ", "House Vote Choice, ",
      "Random Forests, ", "CCES 2006--2018"
    ),
    digits = c(0, 0, 0, 4, 4, 4, 4, 4, 4)
  )

print(
  tab, include.rownames = FALSE,
  file = "tab/CCES_house_rf.tex"
)

## Senate Vote Choice, Random Forest ===========================================
tab <- seq(4) %>%
  map(~ perf_summ("senate", "rf", .x)) %>%
  bind_rows(.id = "Specification") %>%
  arrange(desc(Year), Specification) %>%
  xtable(
    label = "tab:cces_house_rf",
    caption = paste0(
      "Performance Metrics, ", "Senate Vote Choice, ",
      "Random Forests, ", "CCES 2006--2018"
    ),
    digits = c(0, 0, 0, 4, 4, 4, 4, 4, 4)
  )

print(
  tab, include.rownames = FALSE,
  file = "tab/CCES_senate_rf.tex"
)

