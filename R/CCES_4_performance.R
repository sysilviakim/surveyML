source(here::here("R", "utilities.R"))

## Extract performance measures ================================================
## loop to enable gc()
perf <- list()
vid <- list()

for (yr in rev(seq(2006, 2018, 2))) {
  for (i in seq(length(file_suffix[[paste0("year", yr)]]))) {
    sfx <- file_suffix[[paste0("year", yr)]][i]
    load(
      here("data", "cces-tidy", paste0("data_", yr, "_", sfx, ".RData"))
    )

    ## caret results
    for (method in c("logit", "cart", "rf")) {
      for (varset in seq(4)) {

        ## Load previously run results
        load(
          here(
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
          here(
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
        save(perf, file = here("output/CCES/CCES_perf.RData"))
        save(vid, file = here("output/CCES/CCES_varimp.RData"))
      }
    }
  }
}

## Export method X y-variable table ============================================
load(here("output/CCES/CCES_perf.RData"))
load(here("output/CCES/CCES_varimp.RData"))

ref <- tibble(
  idx = seq(4),
  caption = c(
    "Presidential Vote Choice, ", "Presidential Approval Rating, ",
    "House Vote Choice, ", "Senate Vote Choice, "
  ),
  filename = c("preschoice", "presapprov", "house", "senate")
)

# xtable export ================================================================
setNames(seq(4), c(1, 2, "house", "senate")) %>%
  imap(
    ~ {
      for (method in c("logit", "cart", "rf")) {
        tab <- seq(4) %>%
          map(
            function(x) perf_summ(perf, ifelse(.x < 3, .x, .y), method, x)
          ) %>%
          bind_rows(.id = "Set") %>%
          arrange(desc(Year), Set) %>%
          mutate(Set = factor(Set, levels = seq(4), labels = set_labels)) %>%
          rename(`Variable Specification` = Set)

        # No pres. choice for 2006 wave
        if (.x == 1) {
          tab <- tab %>% filter(Year != 2006)
        }

        tab <- tab %>%
          select(-AUC_lower, -AUC_upper, -Accuracy_lower, -Accuracy_upper) %>%
          xtable(
            label = paste0("tab:cces_", ref$filename[.x], "_", method),
            caption = paste0(
              "Performance Metrics, ", ref$caption[.x],
              case_when(
                method == "rf" ~ "Random Forests, ",
                method == "cart" ~ "CART, ",
                method == "logit" ~ "Logit, "
              ),
              ifelse(.x == 1, "CCES 2008--2018", "CCES 2006--2018")
            ),
            digits = c(0, 0, 0, 4, 4, 4, 4, 4, 4)
          )

        print(
          tab,
          include.rownames = FALSE,
          file = here(
            "tab", paste0("CCES_", ref$filename[.x], "_", method, ".tex")
          ),
          booktabs = TRUE
        )
      }
    }
  )

# Alternative, just accuracy between methods ===================================
for (sfx in c(prezvote = 1, house = 3, senate = 4)) {
  tab <- c("logit", "cart", "rf") %>%
    map(
      ~ seq(4) %>%
        map_dfr(
          function(x) perf_summ(
            within(perf, rm("year2006")), sfx, .x, x,
            yr = rev(seq(2008, 2018, 2))
          ) %>%
            select(Year, !!as.name(.x) := CI) %>%
            mutate(Set = x)
        ) %>%
        select(Set, everything()) %>%
        arrange(desc(Year), Set) %>%
        mutate(Set = factor(Set, levels = seq(4), labels = set_labels)) %>%
        rename(`Variable Specification` = Set)
    ) %>%
    Reduce(left_join, .) %>%
    rename(Logit = logit, CART = cart, RF = rf) %>%
    xtable(
      label = paste0("tab:CCES_", sfx, "_accuracy"),
      caption = paste0(
        "Accuracy Range Comparison, ",
        paste0(
          ifelse(sfx == 1, "Presidential", ifelse(sfx == 3, "House", "Senate")),
          " Vote Choice, "
        ),
        "CCES 2008--2018"
      ),
      digits = 0
    )
  
  print(
    tab,
    include.rownames = FALSE,
    file = here("tab", paste0("CCES_", ref$filename[sfx], "_accuracy.tex")),
    booktabs = TRUE, floating = FALSE, tabular.environment = "longtable"
  )
}