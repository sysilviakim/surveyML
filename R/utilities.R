## Tidyverse ===================================================================
library(plyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(forcats)
library(lubridate)
library(foreign)
library(haven)
library(here)

## Other Packages ==============================================================
library(janitor)
library(assertthat)
library(caret)
library(rattle)
library(ROCR)
library(pROC)
library(doParallel)
library(ranger) ## Added from Jan's code
library(randomForestExplainer) ## Added from Jan's code
library(Kmisc) ## devtools::install_github("sysilviakim/Kmisc")
library(xtable)

## Functions ===================================================================
file_path_fxn <- function(data = "CCES") {
  file.path(
    "output", data,
    method, paste0(method, "_", yr, "_", sfx, "_st", varset, ".RData")
  )
}

one_hot <- function(df) {
  x <- predict(caret::dummyVars(~., df, fullRank = TRUE), df)
  output <- as_tibble(x)
  return(output)
}

data_routine <- function(df, dep, lvl, lbl, dbl = NULL, na = 999, seed = 100) {
  ## Turn NA into a certain value that is not used as a response in survey
  ## CCES and ANES is okay with 999; not Nationscape
  df[df == "\"\""] <- ""
  if (!is.null(na)) {
    df[is.na(df)] <- na
  }

  ## Keep only non-NA values for depvar
  df <- df %>% filter(!is.na(!!as.name(dep)))

  ## Zero-variance columns or columns with many responses, except dbl
  ## e.g. birth year
  temp <- df %>%
    map(~ (length(unique(.x)) < 2 | length(unique(.x)) > 20)) %>%
    unlist() %>%
    which()
  if (length(setdiff(temp, match(dbl, names(df), nomatch = 0))) > 0) {
    df <- df[, -setdiff(temp, match(dbl, names(df), nomatch = 0))] %>%
      ## If missing continuous values, delete row
      filter_at(dbl, all_vars(!is.na(.)))
  }

  ## Near-zero variance variables
  temp <- caret::nearZeroVar(df, freqCut = 99 / 1)
  if (length(temp) > 0) {
    df <- df[, -temp]
  }

  ## Strip character-variables, turn variables cardinal (prep for one-hot enc)
  temp <- c(
    which((df %>% map(class) %>% unlist()) == "AsIs"),
    which((df %>% map(class) %>% unlist()) == "character")
  )
  if (length(temp) > 0) {
    df <- df[, -temp]
  }
  df <- df %>%
    mutate_if(is.integer, as.factor) %>%
    mutate_if(is.numeric, as.factor) %>%
    mutate_at(tidyselect::all_of(intersect(dbl, names(df))), as.numeric)

  ## Set up the dependent variable
  df$depvar <- factor(df[[dep]], levels = lvl, labels = lbl)
  df <- df %>%
    select(depvar, everything()) %>%
    select(-!!as.name(dep))

  ## One-hot encoding, then fix the depvar
  df <- one_hot(df)
  names(df)[grepl("depvar", names(df))] <- "depvar"
  df$depvar <- factor(df$depvar, levels = c(0, 1), labels = lbl)

  ## Keep only non-NA values for depvar
  df <- df %>% filter(!is.na(depvar))

  ## Partition training and testing data (80:20 default, and 50:50)
  set.seed(seed)
  index0.8 <- caret::createDataPartition(df$depvar, p = 0.8, list = FALSE)
  set.seed(seed)
  index0.5 <- caret::createDataPartition(df$depvar, p = 0.5, list = FALSE)

  return(
    list(
      train = df[index0.8[, ], ],
      test = df[-index0.8[, ], ],
      index = index0.8[, ],
      train_robust = df[index0.5[, ], ],
      test_robust = df[-index0.5[, ], ],
      index_robust = index0.5[, ]
    )
  )
}

p_font <- function(p, font, size) {
  p +
    theme(
      plot.title = element_text(family = font),
      text = element_text(family = font),
      axis.text.x = element_text(family = font),
      axis.text.y = element_text(family = font),
      legend.text = element_text(family = font),
      strip.text.x = element_text(size = size)
    ) +
    scale_color_continuous()
}

train_name_clean <- function(temp) {
  temp$train <- clean_names(temp$train)
  temp$test <- clean_names(temp$test)
  temp$train_robust <- clean_names(temp$train_robust)
  temp$test_robust <- clean_names(temp$test_robust)
  return(temp)
}

train_1line <- function(temp, metric = "ROC", method = "rpart", tc = NULL,
                        seed = 100) {
  set.seed(seed)

  if (is.null(tc)) {
    tc <- trainControl(
      method = "cv",
      number = 10,
      ## Provides ROC summary stats
      summaryFunction = caret::twoClassSummary,
      allowParallel = TRUE,
      verboseIter = FALSE,
      seeds = rep_seeds(),
      classProbs = TRUE,
      savePredictions = "final"
    )
  }

  ## https://topepo.github.io/caret/available-models.html
  mtrys <- c(
    floor(sqrt(ncol(temp$train %>% select(-depvar)))),
    2,
    floor((2 + (dim(temp$train)[2] - 1)) / 2),
    dim(temp$train)[2]
  )
  if (method == "rf" | method == "ranger") {
    out <- train(
      as.factor(depvar) ~ .,
      metric = metric,
      method = "ranger",
      trControl = tc,
      importance = "permutation",
      tuneLength = 10,
      data = temp$train
    )
  } else if (method == "rpart") {
    out <- train(
      as.factor(depvar) ~ .,
      metric = metric,
      method = method,
      trControl = tc,
      data = temp$train
    )
  } else if (method == "lm") {
    out <- train(
      as.numeric(depvar) ~ .,
      metric = "RMSE",
      method = method,
      trControl = trainControl(
        method = "cv",
        number = 10,
        allowParallel = TRUE,
        verboseIter = FALSE,
        seeds = rep_seeds(),
        classProbs = TRUE
      ),
      data = temp$train
    )
  } else if (method == "logit") {
    out <- train(
      as.factor(depvar) ~ .,
      metric = metric,
      method = "glm",
      family = "binomial",
      trControl = tc,
      data = temp$train
    )
  } else if (method == "lasso") {
    out <- train(
      as.factor(depvar) ~ .,
      metric = metric,
      method = "glmnet",
      trControl = tc,
      ## recheck tuning later
      tuneGrid = expand.grid(alpha = 1, lambda = seq(0.01, 1, by = 0.1)),
      data = temp$train
    )
  }
  return(out)
}

pdf_varimp <- function(x, filename, labels = NULL, font = NULL, size = 12) {
  if (is.null(font)) font <- "Kievit Offc Pro"
  temp <- varImp(x)
  set.seed(100)
  ## Only actually important variables. Not zeros.
  p <- ggplot(
    temp,
    top = min(10, nrow(temp$importance %>% filter(Overall > 0)))
  )
  if (!is.null(labels)) {
    p <- p + scale_x_discrete("Feature", labels = labels)
  }
  pdf(filename, width = 8, height = 4)
  print(p_font(Kmisc::pdf_default(p), font = font, size = size))
  dev.off()
}

cartplot_pdf <- function(x, filename) {
  pdf(filename, width = 6, height = 3)
  fancyRpartPlot(x$finalModel, caption = "")
  dev.off()
}

perf_routine <- function(method, x, test, dv, verbose = TRUE) {
  ## https://stackoverflow.com/questions/30097730/
  ## Per Kuhn's advice, fixing code
  ## if (method == "rf") x <- x$finalModel
  output <- list()
  output[["method"]] <- method

  ## So that we do not invoke stats::predict
  output[["pred.probs"]] <- pred.probs <- predict.train(x, test, type = "prob")
  p1 <- pred.probs[, dv[1]]
  p2 <- pred.probs[, dv[2]]

  output[["pred.factor"]] <- pred.factor <- factor(
    ifelse(p2 < 0.5, dv[1], dv[2]),
    levels = dv
  )

  ## Confusion matrix contains accuracy (+CI)
  output[["cf.matrix"]] <- confusionMatrix(pred.factor, test$depvar)
  output[["pred.compare"]] <- pred.compare <- prediction(
    p2, as.numeric(test$depvar) - 1
  )
  output[["auc"]] <- ROCR::performance(pred.compare, "auc")@y.values[[1]]

  output[["ci.auc.delong"]] <- ci.auc(as.numeric(test$depvar) - 1, p1)
  output[["ci.auc.bootstrap"]] <- ci.auc(
    as.numeric(test$depvar) - 1, p1,
    method = "bootstrap"
  )

  output[["perf"]] <- ROCR::performance(pred.compare, "tpr", "fpr")
  output[["prec"]] <- ROCR::performance(pred.compare, "prec", "rec")
  output[["f1"]] <- ROCR::performance(pred.compare, "f")

  if (verbose) {
    message("Performance metrics for year ", yr, " ", sfx, ": ", output$auc)
  }

  return(output)
}

perf_summ <- function(perf, dv, method, set, yr = rev(seq(2006, 2018, 2))) {
  data.frame(
    Year = yr,
    AUC = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$auc) %>% unlist(),
    AUC_lower = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$ci.auc.bootstrap) %>% map(1) %>% unlist(),
    ## map(2) ---> AUC
    AUC_upper = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$ci.auc.bootstrap) %>% map(3) %>% unlist(),
    Accuracy = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$cf.matrix$overall[["Accuracy"]]) %>%
      unlist(),
    Accuracy_lower = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$cf.matrix$overall[["AccuracyLower"]]) %>%
      unlist(),
    Accuracy_upper = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$cf.matrix$overall[["AccuracyUpper"]]) %>%
      unlist(),
    CI = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$cf.matrix$overall) %>%
      map(
        ~ paste0(
          "[",  
          str_pad(round(.x[["AccuracyLower"]], digits = 4), 6, "right", "0"),
          ", ", 
          str_pad(round(.x[["AccuracyUpper"]], digits = 4), 6, "right", "0"), 
          "]"
        )
      ) %>%
      unlist(),
    Precision = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$cf.matrix$byClass[c("Precision")]) %>%
      map(~ round(.x, digits = 4)) %>%
      unlist(),
    Recall = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$cf.matrix$byClass[c("Recall")]) %>%
      map(~ round(.x, digits = 4)) %>%
      unlist(),
    F1 = perf %>% map(dv) %>% map(method) %>% map(set) %>%
      map(~ .x$cf.matrix$byClass[c("F1")]) %>%
      map(~ round(.x, digits = 4)) %>%
      unlist(),
    row.names = NULL
  )
}

vi_fin <- function(x, names = "Demographics", yrs = seq(1952, 2016, by = 4),
                   lvl = c("Black", "Hispanic", "Gender", "Age")) {
  x %>%
    bind_rows(.id = "Year") %>%
    mutate(Year = as.integer(gsub("year", "", Year))) %>%
    pivot_longer(
      -Year,
      names_to = names, values_to = "Variable Importance"
    ) %>%
    mutate(
      !!as.name(names) := factor(!!as.name(names), levels = lvl)
    ) %>%
    ggplot(
      aes(
        x = Year, y = `Variable Importance`, group = !!as.name(names),
        colour = !!as.name(names), fill = !!as.name(names),
        linetype = !!as.name(names)
      )
    ) +
    scale_x_continuous(breaks = yrs) +
    geom_line() +
    scale_color_discrete() +
    scale_linetype_discrete()
}

vi_bottom <- function(p, nrow = 2, key = 1, kh = .5) {
  p +
    theme(legend.position = "bottom", legend.key.width = unit(key, "cm")) +
    guides(
      colour = guide_legend(nrow = nrow, byrow = TRUE, keyheight = kh),
      linetype = guide_legend(nrow = nrow, byrow = TRUE, keyheight = kh)
    )
}

po_plot <- function(x, metric, years = seq(2008, 2020, by = 2),
                    ylim = c(0.38, 1.0)) {
  p <- ggplot(
    x, aes(
      x = Year, y = !!as.name(metric), group = Set, colour = Set, linetype = Set
    )
  ) +
    geom_line(size = 1) +
    scale_x_continuous(breaks = years) +
    scale_color_viridis_d(
      direction = -1, name = "Variable Specification", end = 0.85
    ) +
    scale_linetype_manual(
      name = "Variable Specification",
      values = c("solid", "twodash", "dotted", "dashed")
    ) +
    guides(
      colour = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )

  if (!is.null(ylim)) {
    p <- p + scale_y_continuous(limits = ylim)
  }
  return(p)
}

po_full <- function(x, metric) {
  ggplot(
    x, aes(x = Year, y = !!as.name(metric), colour = Set, linetype = Survey)
  ) +
    geom_line(size = 1) +
    scale_x_continuous(breaks = seq(1952, 2020, by = 4)) +
    scale_color_viridis_d(
      direction = -1, name = "Specification", end = 0.85
    ) +
    scale_linetype_manual(name = "Survey", values = c("solid", "dashed")) +
    guides(
      colour = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    scale_y_continuous(limits = c(0.38, 1.0))
}

# Within-year, between-set intersection fxn
vi_inter_btw <- function(x, y = 1, from = 1, to = 4, top = 20,
                         pid_vec, educ_vec, survey = "CCES") {
  out <- x %>%
    map(y) %>%
    map("rf") %>%
    map(
      ~ Reduce(
        intersect,
        .x %>%
          map(
            function(x) {
              x %>%
                arrange(desc(Overall)) %>%
                slice_head(n = top) %>%
                .$rownames
            }
          ) %>%
          .[from:to]
      )
    ) %>%
    map(
      ~ tibble(x := .x) %>%
        t() %>%
        as_tibble(.name_repair = "unique") %>%
        # Adjusting to new .name_repair conventions
        rename_all(~ gsub("...", "V", .x))
    ) %>%
    bind_rows(.id = "Year") %>%
    mutate(Year = gsub("year", "", Year))

  if (survey == "CCES") {
    out <- out %>%
      mutate_all(~ if_else(. %in% c("gender.2", "V208.2"), "Gender", .)) %>%
      mutate_all(~ if_else(. %in% c("birthyr", "V207", "v207"), "Age", .)) %>%
      mutate_all(
        ~ if_else(. %in% c("race.2", "V211.2", "v211_black"), "Black", .)
      ) %>%
      mutate_all(~ if_else(. %in% c("race.3", "V211.3"), "Hispanic", .)) %>%
      mutate_all(
        ~ if_else(
          . %in% paste0(pid_vec, ".2"), "Not very strong Democrat",
          if_else(
            . %in% paste0(pid_vec, ".3"), "Lean Democrat",
            if_else(
              . %in% paste0(pid_vec, ".4"), "Independent",
              if_else(
                . %in% paste0(pid_vec, ".5"), "Lean Republican",
                if_else(
                  . %in% paste0(pid_vec, ".6"), "Not very strong Republican",
                  if_else(
                    . %in% paste0(pid_vec, ".7"), "Strong Republican", .
                  )
                )
              )
            )
          )
        )
      ) %>%
      mutate_all(
        ~ if_else(
          . %in% paste0(educ_vec, ".2"), "High school graduate",
          if_else(
            . %in% c(paste0(educ_vec, ".3"), "v213_some_college"),
            "Some college",
            if_else(
              . %in% c(paste0(educ_vec, ".4"), "v213_2_year"), "2-year college",
              if_else(
                . %in% c(paste0(educ_vec, ".5"), "v213_4_year"),
                "4-year college",
                if_else(
                  . %in% paste0(educ_vec, ".6"), "Post-grad", .
                )
              )
            )
          )
        )
      )
  } else if (survey == "ANES") {
    out <- out %>%
      mutate_all(~ if_else(. %in% c("vcf0104_2"), "Gender", .)) %>%
      mutate_all(~ if_else(. %in% c("vcf0101"), "Age", .)) %>%
      mutate_all(~ if_else(. %in% c("vcf0105b_2"), "Black", .)) %>%
      mutate_all(~ if_else(. %in% c("vcf0105b_3"), "Hispanic", .)) %>%
      mutate_all(~ if_else(. %in% c("vcf0105b_4"), "Other Race", .)) %>%
      mutate_all(
        ~ if_else(
          . %in% paste0(pid_vec, "_2"), "Not very strong Democrat",
          if_else(
            . %in% paste0(pid_vec, "_3"), "Lean Democrat",
            if_else(
              . %in% paste0(pid_vec, "_4"), "Independent",
              if_else(
                . %in% paste0(pid_vec, "_5"), "Lean Republican",
                if_else(
                  . %in% paste0(pid_vec, "_6"), "Not very strong Republican",
                  if_else(
                    . %in% paste0(pid_vec, "_7"), "Strong Republican", .
                  )
                )
              )
            )
          )
        )
      ) %>%
      mutate_all(
        ~ if_else(
          . %in% paste0(educ_vec, "_2"), "High school graduate",
          if_else(
            . %in% c(paste0(educ_vec, "_3")), "Some college",
            if_else(
              . %in% c(paste0(educ_vec, "_4")), "2-year college",
              if_else(
                . %in% c(paste0(educ_vec, "_5")), "4-year college",
                if_else(
                  . %in% paste0(educ_vec, "_6"), "Post-grad", .
                )
              )
            )
          )
        )
      ) %>%
      mutate_all(
        ~ if_else(
          . %in% c("vcf0114_2"), "Income: 17-33 %tile",
          if_else(
            . %in% c("vcf0114_3"), "Income: 34-67 %tile",
            if_else(
              . %in% c("vcf0114_4"), "Income: 68-95 %tile",
              if_else(
                . %in% c("vcf0114_5"), "Income: 96-100 %tile",
                if_else(
                  . %in% c("vcf0114_999"), "Income: refused", .
                )
              )
            )
          )
        )
      )
  }
  return(out)
}

# Between-year, within-set intersection fxn
vi_inter_within <- function(x, y = 1, set = 4, top = 20) {
  x %>%
    map(y) %>%
    map("rf") %>%
    map(set) %>%
    map(
      ~ .x %>%
        arrange(desc(Overall)) %>%
        slice_head(n = top) %>%
        .$rownames
    )
  ## Much more manual tagging, I'm afraid.
}

# Varimp over time fxns (CCES)
vi_ts_demo <- function(x, y = 1, set = 4, method = "rf",
                       names = "Demo.", yrs = seq(2008, 2018, by = 2)) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(
            rownames %in% c("gender.2", "v208_female", "V208.2", "v2004.2")
          ) %>%
          select(Gender = Overall),
        .x %>%
          filter(rownames %in% c("birthyr", "V207", "v207")) %>%
          select(Age = Overall),
        .x %>%
          filter(rownames %in% c("race.2", "V211.2", "v211_black")) %>%
          select(Black = Overall),
        .x %>%
          filter(rownames %in% c("race.3", "V211.3", "v211_hispanic")) %>%
          select(Hispanic = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs)
}

vi_ts_edu <- function(x, y = 1, set = 4, method = "rf",
                      names = "Demo.", yrs = seq(2008, 2018, by = 2),
                      lvl = c(
                        "HS Graduate", "Some College",
                        "2-year", "4-year", "Post-grad"
                      )) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(
            rownames %in% c("educ.2", "v213_high_school_graduate", "V213.2")
          ) %>%
          select(`HS Graduate` = Overall),
        .x %>%
          filter(rownames %in% c("educ.3", "v213_some_college", "V213.3")) %>%
          select(`Some College` = Overall),
        .x %>%
          filter(rownames %in% c("educ.4", "v213_2_year", "V213.4")) %>%
          select(`2-year` = Overall),
        .x %>%
          filter(rownames %in% c("educ.5", "v213_4_year", "V213.5")) %>%
          select(`4-year` = Overall),
        .x %>%
          filter(rownames %in% c("educ.6", "v213_post_grad", "V213.6")) %>%
          select(`Post-grad` = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs, lvl = lvl)
}

vi_ts_pid <- function(x, y = 1, set = 4, method = "rf", names = "PID",
                      yrs = seq(2008, 2018, by = 2),
                      lvl = rev(c(
                        "Weak Democrat", "Lean Democrat", "Independent",
                        "Lean Republican", "Weak Republican",
                        "Strong Republican"
                      ))) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(
            rownames %in% c(
              "pid7.2", "v212d_not_very_strong_democrat", "CC307a.2"
            )
          ) %>%
          select(`Weak Democrat` = Overall), ## Not very strong Democrat
        .x %>%
          filter(
            rownames %in% c("pid7.3", "v212d_lean_democrat", "CC307a.3")
          ) %>%
          select(`Lean Democrat` = Overall),
        .x %>%
          filter(
            rownames %in% c("pid7.4", "v212d_independent", "CC307a.4")
          ) %>%
          select(`Independent` = Overall),
        .x %>%
          filter(
            rownames %in% c("pid7.5", "v212d_lean_republican", "CC307a.5")
          ) %>%
          select(`Lean Republican` = Overall),
        .x %>%
          filter(
            rownames %in% c(
              "pid7.6", "v212d_not_very_strong_democrat", "CC307a.6"
            )
          ) %>%
          select(`Weak Republican` = Overall), ## Not very strong Republican
        .x %>%
          filter(
            rownames %in% c("pid7.7", "v212d_strong_democrat", "CC307a.7")
          ) %>%
          select(`Strong Republican` = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs, lvl = lvl)
}

# Varimp over time fxns (ANES)
vi_ts_demo2 <- function(x, y = 1, set = 4, method = "rf",
                        names = "Demographics", yrs = seq(1952, 2016, by = 4)) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(grepl("vcf0104_2", rownames)) %>% select(Gender = Overall),
        .x %>%
          filter(grepl("vcf0101", rownames)) %>% select(Age = Overall),
        .x %>%
          filter(grepl("vcf0105b_2", rownames)) %>% select(Black = Overall),
        ## For some years, race beyond white and black not there
        .x %>%
          filter(grepl("vcf0105b_3", rownames)) %>%
          select(Hispanic = Overall) %>%
          bind_rows(., data.frame(Hispanic = NA)) %>%
          slice(1)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs)
}

vi_ts_edu2 <- function(x, y = 1, set = 4, method = "rf",
                       names = "Demographics", yrs = seq(1952, 2016, by = 4),
                       lvl = c(
                         "HS Graduate", "Some College", "College+"
                       )) {
  x %>%
    map(y) %>%
    map(method) %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(grepl("vcf0110_2", rownames)) %>%
          select(`HS Graduate` = Overall),
        .x %>%
          filter(grepl("vcf0110_3", rownames)) %>%
          select(`Some College` = Overall),
        .x %>%
          filter(grepl("vcf0110_4", rownames)) %>%
          select(`College+` = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs, lvl = lvl)
}

vi_ts_pid2 <- function(x, y = 1, set = 4, names = "PID",
                       yrs = seq(1952, 2016, by = 4),
                       lvl = rev(c(
                         "Weak Democrat", "Lean Democrat", "Independent",
                         "Lean Republican", "Weak Republican",
                         "Strong Republican"
                       ))) {
  x %>%
    map(y) %>%
    map("rf") %>%
    map(set) %>%
    map(
      ~ bind_cols(
        .x %>%
          filter(grepl("vcf0301_2", rownames)) %>%
          select(`Weak Democrat` = Overall),
        .x %>%
          filter(grepl("vcf0301_3", rownames)) %>%
          select(`Lean Democrat` = Overall),
        .x %>%
          filter(grepl("vcf0301_4", rownames)) %>%
          select(`Independent` = Overall),
        .x %>%
          filter(grepl("vcf0301_5", rownames)) %>%
          select(`Lean Republican` = Overall),
        .x %>%
          filter(grepl("vcf0301_6", rownames)) %>%
          select(`Weak Republican` = Overall),
        .x %>%
          filter(grepl("vcf0301_7", rownames)) %>%
          select(`Strong Republican` = Overall)
      )
    ) %>%
    vi_fin(names = names, yrs = yrs, lvl = lvl)
}

## Extra Setup =================================================================
options(
  mc.cores = parallel::detectCores(),
  java.parameters = "-Xmx32g",
  digits = 4,
  scipen = 100
)

set_labels <- c(
  "Demographics Only", "Demo. + PID", "Demo. + PID + Issues", "All Covariates"
)

### Jan's fit_control_basic equivalent
tc <- trainControl(
  method = "cv",
  number = 10,
  ## Provides ROC summary stats
  summaryFunction = caret::twoClassSummary,
  allowParallel = TRUE,
  verboseIter = FALSE,
  seeds = rep_seeds(),
  classProbs = TRUE,
  savePredictions = "final"
)

file_suffix <- list(
  year2018 = c("2016vote", "trumpapproval"),
  year2016 = c("2016vote", "obamaapproval"),
  year2014 = c("2012vote", "obamaapproval"),
  year2012 = c("2012vote", "obamaapproval"),
  year2010 = c("2008vote", "obamaapproval"),
  year2008 = c("2008vote", "bushapproval"),
  year2006 = c()
) %>%
  map(~ c(.x, c("house", "senate")))

## Label for CCES 2018 ---------------------------------------------------------
vl <- list(
  common = c(
    "pid3.2" = "3-pt Party ID: I think of myself as Republican",
    "pid7.2" = "7-pt Party ID: Not very strong Democrat",
    "pid7.3" = "7-pt Party ID: Lean Democrat",
    "pid7.4" = "7-pt Party ID: Independent",
    "pid7.5" = "7-pt Party ID: Lean Republican",
    "pid7.6" = "7-pt Party ID: Not very strong Republican",
    "pid7.7" = "7-pt Party ID: Strong Republican",
    "pid7.8" = "7-pt Party ID: Not Sure",
    "pid7.999" = "7-pt Party ID: Not Asked/Skipped",
    "gender.2" = "Gender",
    "race.2" = "Black",
    "race.3" = "Hispanic",
    "race.4" = "Asian",
    "race.5" = "Native American",
    "race.8" = "Middle Eastern",
    "race.6" = "Mixed",
    "race.7" = "Other",
    "educ.2" = "Education: High School Graduate",
    "educ.3" = "Education: Some College",
    "educ.4" = "Education: 2-year",
    "educ.5" = "Education: 4-year",
    "educ.6" = "Education: Post-grad",
    "faminc_new.2" = "Income: $10,000-$19,999",
    "faminc_new.3" = "Income: $20,000-$29,999",
    "faminc_new.4" = "Income: $30,000-$39,999",
    "faminc_new.5" = "Income: $40,000-$49,999",
    "faminc_new.6" = "Income: $50,000-$59,999",
    "faminc_new.7" = "Income: $60,000-$69,999",
    "faminc_new.8" = "Income: $70,000-$79,999",
    "faminc_new.9" = "Income: $80,000-$99,999",
    "faminc_new.10" = "Income: $100,000-$119,999",
    "faminc_new.11" = "Income: $120,000-$149,999",
    "faminc_new.31" = "Income: $150,000 or more",
    "faminc_new.12" = "Income: $150,000-$199,999",
    "faminc_new.13" = "Income: $200,000-$249,999",
    "faminc_new.14" = "Income: $250,000-$349,999",
    "faminc_new.15" = "Income: $350,000-$499,999",
    "faminc_new.16" = "Income: $500,000 or more",
    "faminc_new.97" = "Income: Prefer Not to Say",
    "faminc.32" = "Income: $250,000 or more",
    "faminc.2" = "Income: $10,000-$19,999",
    "faminc.3" = "Income: $20,000-$29,999",
    "faminc.4" = "Income: $30,000-$39,999",
    "faminc.5" = "Income: $40,000-$49,999",
    "faminc.6" = "Income: $50,000-$59,999",
    "faminc.7" = "Income: $60,000-$69,999",
    "faminc.8" = "Income: $70,000-$79,999",
    "faminc.9" = "Income: $80,000-$99,999",
    "faminc.10" = "Income: $100,000-$119,999",
    "faminc.11" = "Income: $120,000-$149,999",
    "faminc.31" = "Income: $150,000 or more",
    "faminc.12" = "Income: $150,000-$199,999",
    "faminc.13" = "Income: $200,000-$249,999",
    "faminc.14" = "Income: $250,000-$349,999",
    "faminc.15" = "Income: $350,000-$499,999",
    "faminc.16" = "Income: $500,000 or more",
    "faminc.32" = "Income: $250,000 or more",
    "faminc.97" = "Income: Prefer Not to Say",
    "birthyr" = "Birth year",
    "employ.2" = "Employment: Part-time",
    "employ.3" = "Employment: Temporarily Laid Off",
    "employ.4" = "Employment: Unemployed",
    "employ.5" = "Employment: Retired",
    "employ.6" = "Employment: Permanently disabled",
    "employ.7" = "Employment: Homemaker",
    "employ.8" = "Employment: Student",
    "employ.9" = "Employment: Other",
    "V208.2" = "Gender",
    "V211.2" = "Black",
    "V211.3" = "Hispanic",
    "V211.4" = "Asian",
    "V211.5" = "Native American",
    "V211.8" = "Middle Eastern",
    "V211.6" = "Mixed",
    "V211.7" = "Other",
    "V213.2" = "Education: High School Graduate",
    "V213.3" = "Education: Some College",
    "V213.4" = "Education: 2-year",
    "V213.5" = "Education: 4-year",
    "V213.6" = "Education: Post-grad",
    "V246.2" = "Income: $10,000-$14,999",
    "V246.3" = "Income: $15,000-$19,999",
    "V246.4" = "Income: $20,000-$24,999",
    "V246.5" = "Income: $25,000-$29,999",
    "V246.6" = "Income: $30,000-$39,999",
    "V246.7" = "Income: $40000-$49,999",
    "V246.8" = "Income: $50,000-$59,999",
    "V246.9" = "Income: $60,000-$69,999",
    "V246.10" = "Income: $70,000-$79,999",
    "V246.11" = "Income: $80,000-$99,999",
    "V246.12" = "Income: $100,000-$119,999",
    "V246.13" = "Income: $120,000-$149,999",
    "V246.14" = "Income: $150,000 or more",
    "V246.15" = "Prefer Not to Say",
    "V207" = "Birth Year"
  )
)

vl[["var18"]] <- c(
  "CC18_301.2" = "Believes national economy has gotten better",
  "CC18_301.3" = "Believes national economy has stayed about the same",
  "CC18_301.4" = "Believes national economy has gotten worse",
  "CC18_301.5" = "Believes national economy has gotten much worse",
  "CC18_301.6" = "Not sure whether national economoy has gotten better/worse",
  "CC18_308a.2" = "Somewhat approve of the way Trump is doing his job",
  "CC18_308a.3" = "Somewhat disapprove of the way Trump is doing his job",
  "CC18_308a.4" = "Strongly disapprove of the way Trump is doing his job",
  "CC18_308b.2" = "Somewhat approve of the way Congress is doing job",
  "CC18_308b.3" = "Somewhat disapprove of the way Congress is doing job",
  "CC18_308b.4" = "Strongly disapprove of the way Congress is doing job",
  "CC18_308b.5" = "Not sure about the way Congress is doing job",
  "CC18_309c.3" = "(Believes that neither party has majority in State Senate)",
  "CC18_317.2"  = "Voted for Clinton in 2016",
  "CC18_317.3"  = "Voted for 3rd candidate for president in 2016",
  "CC18_318a.1" = "Voted in Democratic Primary",
  "CC18_318a.2" = "Voted in Republican Primary",
  "CC18_318a.5" = "Voted in state's primary elections",
  "CC18_320c.2" = "Against banning assult rifles",
  "CC18_322a.2" = "Oppose increasing spending on border security",
  "CC18_327a.2" = "Oppose providing Medicare for all Americans",
  "CC18_327c.2" = "Oppose repealing the entire Affordable Care Act",
  "CC18_328f.2" = "Would have voted against Kavanaugh",
  "CC18_331a.2" = "Oppose $50 billion tariffs on goods imported from China",
  "CC18_331b.2" = "Oppose construction of XL pipeline",
  "CC18_331c.2" = "Oppose -25% tariff on imported steel and 10% imported aluminum",
  "CC18_332a.2" = "Oppose recognizing Jerusalem as capital of Israel/moving US Embassy",
  "CC18_332c.2" = "Oppose Executive Orders to withdraw from Paris Climate Agreement",
  "CC18_334J.2" = "I would rate Senate candidate 2 liberal",
  "CC18_334J.3" = "I would rate Senate candidate 2 somewhat liberal",
  "CC18_334J.4" = "I would rate Senate candidate 2 middle of the road",
  "CC18_334J.5" = "I would rate Senate candidate 2 somewhat conservative",
  "CC18_334J.6" = "I would rate Senate candidate 2 conservative",
  "CC18_334J.7" = "I would rate Senate candidate 2 very conservative",
  "CC18_334J.8" = "Not sure about Senate candidate 2 ideology",
  "CC18_334M.3" = "Rate House candidate 1 as somewhat liberal",
  "CC18_334M.4" = "Rate House candidate 1 as middle of the road",
  "CC18_334M.999" = "Not sure about House candidate 1 ideology",
  "CC18_334N.999" = "Not sure about House candidate 2 ideology",
  "CC18_334O.7" = "Rates current House member as very conservative",
  "CC18_334O.8" = "Not sure about Current House member's ideological placement",
  "CC18_334O.999" = "(Did not answer ideological placement of current House member)",
  "CC18_335.2" =  "Thinks that Trump colluded w/ Russians to influence 2016 election",
  "CC18_335.3" =  "Not sure whether Trump colluded w/ Russians to influence 2016 election",
  "CC18_350.5" = "Don't intend to vote in 2018",
  "CC18_351.2" = "(Prefers Senate Candidate 2)",
  "CC18_352x.3" = "Voted for 3rd Governor candidate",
  "CC18_353.2"  = "(Prefers House Candidate 2)",
  "CC18_353.8"  = "Not sure about House candidate preference",
  "CC18_354a_6.2" = "Latin heritage: not Dominican Republic",
  "CC18_410b.2" = "Voted for Senate Candidate 2",
  "CC18_410b_nv.2" = "Prefer Senate Candidate 2",
  "CC18_411.999" = "(Did not answer: whom did you vote for Governor?)",
  "CC18_412.2"  = "Voted for House Candidate 2",
  "CC18_412a.2"  = "Hypothetically, would vote for House challenger than incumbent (who is retiring)",
  "CC18_412b.2"  = "Hypothetically, would vote for House challenger than incumbent (who is retiring)",
  "CC18_413c.2" = "Voted Republican in State Senate",
  "CC18_413d.2" = "Voted Republican in State House",
  "CC18_413d.5" = "There was no State House race",
  "CC18_415a.2"  = "Oppose: give EPA power to regulate CO2 emissions",
  "CC18_416.2"  = "Opposes the Financial CHOICE act",
  "CC18_416.999" = "(Did not answer support/oppose of the  Financial CHOICE Act)",
  "CC18_417_c.2" = "Oppose banning immigrants from Iran and others for 90 days",
  "CC18_417a_2.999" = "(Did not answer: put up political sign last year)",
  "CC18_418bx_7.2"  = "Never ran for office in State legislature",
  "CC18_418bx_9.999" = "(Not answered: ran for US House/Senate?)",
  "CC18_420a.999" = "(Did not answer: whom did you vote for Attorney General?)",
  "CC18_420b.2" = "Voted for Secretary of State candidate 2",
  "CC18_420b.999" = "(Did not answer: whom did you vote for Secretary of State?)",
  "CC18_420c.999" = "(Did not answer: whom did you vote for Treasurer?)",
  "CC18_421_dem.999" = "(Did not answer: would you call yourself strong/not so strong Dem)",
  "CC18_421_rep.999" = "(Did not answer: would you call yourself strong/not so strong Rep)",
  "CC18_421a.2" = "I think of myself as Republican (post-wave)",
  "CC18_421b.2" = "I am closer to the Republican party",
  "CC18_423a.6" = "Not sure about race of House candidate 1",
  "CC18_423b.6" = "Not sure about race of House candidate 2",
  "CC18_423b.999" = "(Did not answer: race of House candidate 2)",
  "CC18_423c.4" = "(Believes current House member is Asian)",
  "CC18_app_dtrmp_post.2" = "Somewhat approve of way of Trump handles job as President",
  "CC18_app_dtrmp_post.3" = "Somewhat disapprove of way of Trump handles job as President",
  "CC18_app_dtrmp_post.4" = "Strongly disapprove of way Trump is doing his job (post-wave)",
  "CC18_app_dtrmp_post.5" = "Not sure about way Trump is doing his job (post-wave)",
  "CL_party.2"  = "Catalist party affiliation: Constitution party",
  "milstat_4.2" = "No members of my family served in US miliary and no longer active"
) %>%
  c(vl$common, .)

## Label for CCES 2016 ---------------------------------------------------------
vl[["var16"]] <- c(
  "CC16_302.2" = "Last year national economy has: gotten better",
  "CC16_302.3" = "Last year national economy has: stayed about the same",
  "CC16_302.4" = "Last year national economy has: gotten worse",
  "CC16_302.5" = "Last year national economy has: gotten much worse",
  "CC16_304.2" = "Project economy next year: got somewhat better",
  "CC16_304.4" = "Project economy next year: got somewhat worse",
  "CC16_304.6" = "Project economy next year: not sure",
  "CC16_320a.3" = "Obama approval: somewhat disapprove",
  "CC16_320a.4" = "Do NOT strongy disapprove of Obama",
  "CC16_320a.5" = "Obama approval: not sure",
  "CC16_320b.3" = "Congress approval: somewhat disapprove",
  "CC16_320b.4" = "Congress approval: do not strongy disapprove",
  "CC16_320b.5" = "Congress approval: not sure",
  "CC16_320c.2" = "Supreme court approval: somewhat approve",
  "CC16_320c.3" = "Supreme court approval: somewhat disapprove",
  "CC16_320c.4" = "Supreme court approval: strongly approve",
  "CC16_320c.5" = "Supreme court approval: not sure",
  "CC16_320d.5" = "Governor approval: not sure",
  "CC16_320d.999" = "Governor approval: skipped",
  "CC16_320e.5" = "Legislature approval: not sure",
  "CC16_320e.999" = "Legislature approval: skipped",
  "CC16_320f.2" = "Rep approval: somewhat approve",
  "CC16_320f.5" = "Rep Party approval: not sure",
  "CC16_320g.4" = "Own-state Senator 1 approval: strongly disapprove",
  "CC16_320g.5" = "Own-state Senator 1 approval: not sure",
  "CC16_320g.999" = "Own-state Senator 1 approval: skipped",
  "CC16_320h.4" = "Own-state Senator 2 approval: strongly disapprove",
  "CC16_320h.5" = "Own-state Senator 2 approval: not sure",
  "CC16_320h.999" = "Own-state Senator 2 approval: skipped",
  "CC16_321a.4" = "Not sure which party has a majority of seats: House",
  "CC16_321b.4" = "Not sure which party has a majority of seats: Senate",
  "CC16_321c.4" = "Not sure which party has a majority of seats: State Senate",
  "CC16_321c.999" = "Skipped which party has a majority of seats: State Senate",
  "CC16_321d.2" = "Wrong about which party has a majority of seats: Lower Chamber",
  "CC16_321d.4" = "Not sure which party has a majority of seats: Lower Chamber",
  "CC16_321d.999" = "Skipped which party has a majority of seats: Lower Chamber",
  "CC16_322a.3" = "Have heard of Governor name, is Democrat",
  "CC16_326.2" = "Did not vote for Romney in 2012",
  "CC16_326.3" = "Voted for someone else than Obama/Romney in 2012",
  "CC16_326.999" = "Did not vote in 2012",
  "CC16_327.2" = "Did not vote in primaries 2016",
  "CC16_328.2" = "Did not vote for Sanders in Primary",
  "CC16_328.4" = "Voted for Trump in Primary",
  "CC16_328.999" = "Skipped answering primary vote status 2016",
  "CC16_330d.2" = "Support banning assault rifles",
  "CC16_330e.2" = "Against easier concealed gun permit",
  "CC16_331_1.2" = "Grant status to all illegal immigrants who had jobs/taxes/no crime?",
  "CC16_331_2.2" = "Increase patrol for immigration?",
  "CC16_331_3.2" = "Grant status to immigrant children with US high school diploma?",
  "CC16_331_7.2" = "Identify and deport illegal immigration?",
  "CC16_332a.2" = "Support abortion = choice",
  "CC16_332b.2" = "Oppose abortion = only when rape?",
  "CC16_332c.2" = "Oppose abortion = all prohibit after 20th week?",
  "CC16_332d.2" = "Oppose employers declining coverage of abortions in insurance",
  "CC16_332e.2" = "Oppose prohibiting federal funds for abortion",
  "CC16_333a.2" = "Support EPA regulating CO2",
  "CC16_333a.999" = "Skipped EPA regulate CO2",
  "CC16_333b.2" = "Oppose raising required fuel efficiency",
  "CC16_333b.999" = "Skipped raising required fuel efficiency",
  "CC16_333c.2" = "Oppose requiring min. renewable fuel",
  "CC16_333c.999" = "Skipped requiring min. renewable fuel",
  "CC16_333d.2" = "Support strengthening Clean Air Act",
  "CC16_333d.999" = "Did not skip question on strengthening Clean Air Act",
  "CC16_334a.2" = "Oppose eliminating min. sentences for non-violent drug offenders",
  "CC16_334c.2" = "Oppose increasing police 10%",
  "CC16_334d.2" = "Oppose increasing prison sentences for 2+ crimes felons",
  "CC16_335.2" = "Support gay marriage",
  "CC16_337_1.3" = "Cut defense spending << others to cover federal deficit",
  "CC16_337_1.999" = "Did not skip answering: cut defense spending to cover fed. deficit?",
  "CC16_337_2.2" = "Cut domestic spending = 2nd choice to cover federal deficit",
  "CC16_337_2.3" = "Cut domestic spending to cover federal deficit",
  "CC16_337_2.999" = "Skipped answering: cut domestic spending to cover fed. deficit?",
  "CC16_337_3.2" = "Raise taxes = 2nd choice to cover federal deficit",
  "CC16_337_3.3" = "Raise taxes << others to cover federal deficit",
  "CC16_337_3.999" = "Skipped answering raise taxes to cover fed. deficit",
  "CC16_340a.2" = "I would rate myself: liberal",
  "CC16_340a.4" = "I would rate myself: middel of road",
  "CC16_340a.6" = "I would rate myself: conservative",
  "CC16_340a.7" = "I would rate myself: very conservative",
  "CC16_340a.8" = "I would rate myself: not sure",
  "CC16_340b.8" = "Governor political scale = not sure",
  "CC16_340b.999" = "Governor political scale = skipped",
  "CC16_340c.2" = "Obama political scale = liberal",
  "CC16_340c.3" = "Obama political scale = somewhat liberal",
  "CC16_340c.4" = "Obama political scale = middle of the road",
  "CC16_340c.5" = "Obama political scale = somewhat conservative",
  "CC16_340c.7" = "Obama political scale = very conservative",
  "CC16_340c.8" = "Obama political scale = not sure",
  "CC16_340d.3" = "Clinton political scale = somewhat liberal",
  "CC16_340d.6" = "Clinton political scale = conservative",
  "CC16_340d.7" = "Clinton political scale = very conservative",
  "CC16_340d.8" = "Clinton political scale = not sure",
  "CC16_340e.4" = "Trump political scale = middle of the road",
  "CC16_340e.8" = "Trump political scale = not sure",
  "CC16_340g.7" = "Dem Party political scale = very conservative",
  "CC16_340g.8" = "Dem Party political scale = not sure",
  "CC16_340g.999" = "Dem Party political scale = skipped",
  "CC16_340h.7" = "Rep Party political scale = very conservative",
  "CC16_340h.8" = "Rep Party political scale = not sure",
  "CC16_340h.999" = "Rep Party political scale = skipped",
  "CC16_340i.8" = "Supreme Court political scale = not sure",
  "CC16_340i.999" = "Supreme Court political scale = skipped",
  "CC16_340j.999" = "Own-state Senator 1 political scale = skipped",
  "CC16_340k.999" = "Current Senator 2 political scale = skipped",
  "CC16_340l.999" = "Senate candidate 1 political scale = skipped",
  "CC16_340m.3" = "Senate candidate 2 political scale = somewhat liberal",
  "CC16_340m.4" = "Senate candidate 2 political scale = middle of the road",
  "CC16_340m.5" = "Senate candidate 2 political scale = somewhat conservative",
  "CC16_340m.6" = "Senate candidate 2 political scale = conservative",
  "CC16_340m.7" = "Senate candidate 2 political scale = very conservative",
  "CC16_340m.8" = "Senate candidate 2 political scale = not sure",
  "CC16_340m.999" = "Senate candidate 2 political scale = skipped",
  "CC16_340n.3" = "House candidate 1 political scale = somewhat liberal",
  "CC16_340n.4" = "House candidate 1 political scale = somewhat middle of the road",
  "CC16_340n.5" = "House candidate 1 political scale = somewhat conservative",
  "CC16_340n.999" = "House candidate 1 political scale = skipped",
  "CC16_3400.999" = "House candidate 2 political scale = skipped",
  "CC16_340p.999" = "House member political scale = skipped",
  "CC16_350.3" = "(Believes number of SCOTUS appointments were equally divided by Democratic and Republican",
  "CC16_351B.2" = "Vote for Trans-Pacific Partnership Act?",
  "CC16_351F.2" = "Vote for Highway and Transportation Funding Act?",
  "CC16_351I.2" = "Against repealing Affordable Care Act",
  "CC16_351K.2" = "For raising minimum wage",
  "CC16_360.2"  = "Registered with the Democratic Party",
  "CC16_364c.2" = "Prefer Clinton as President of US",
  "CC16_365.2" = "Prefer Senate Candidate 2",
  "CC16_367.2" = "Prefers House Candidate 2",
  "CC16_367a.999" = "(Did Not Answer Second House Candidate Preference)",
  "CC16_410a.2" = "Voted for Trump",
  "CC16_410a_nv.2" = "Preferred Clinto as president (post-wave)",
  "CC16_410b.2" = "Voted for Senate Candidate 2",
  "CC16_412.2"  = "Voted for House Candidate 2",
  "CC16_412.14" = "Voted for House Candidate 11",
  "CC16_413a.2" = "(Voted Republican for Attorney General)",
  "CC16_413b.2" = "(Voted Republican for Secretary of State)",
  "CC16_413c.2" = "(Voted Republican for State Senate)",
  "CC16_413d.2" = "(Voted Republican for State House)",
  "CC16_413d.5" = "There was no State House race",
  "CC16_414_1.2" = "Approve of military deploy to ensure supply of oil?",
  "CC16_414_2.2" = "Approve of military deploy to destroy terrorist camp?",
  "CC16_414_5.2" = "Oppose military deployment to protect allies",
  "CC16_414_7.2" = "Approve of military deploy to ... none?",
  "CC16_415r" = "Cutting spending preferred to raising taxes",
  "CC16_421a.2" = "I think of myself as a Republican",
  "CC16_421b.2" = "I am closer to the Republican party",
  "CC16_421b.3" = "I am closer to neither party",
  "CC16_421_dem.999" = "(Did not answer: would I call myself strong/not so strong Democrat)",
  "CC16_421_rep.999" = "(Did not answer: would I call myself strong/not so strong Republican)",
  "CC16_423a.6" = "Not sure about race of House candidate 1",
  "CC16_423a.999" = "(Did not answer: race of House Candidate 1?)",
  "CC16_423b.6" = "Not sure about race of House candidate 2",
  "CC16_423b.999" = "(Did not answer: race of House candidate 2?)",
  "child18.2" = "No children under 18",
  "healthins_1.2" = "(Health insurance?) Through job or family's employer",
  "healthins_2.2" = "Health insurance through Medicare/Medicaid",
  "healthins_6.2" = "(Health insurance?) Don't have",
  "ideo5.4" = "My political viewpoint is: conservative",
  "ideo5.5" = "My political viewpoint is: very conservative",
  "ideo5.6" = "My political viewpoint is: not sure",
  "immstat.2" = "I am immigrant non-citizen",
  "immstat.4" = "(immstat) I am immigrant second gen",
  "immstat.5" = "(immstat) I am immigrant third gen",
  "inputstate.1" = "Alabama",
  "inputstate.2" = "Alaska",
  "inputstate.4" = "Arizona",
  "inputstate.5" = "Arkansas",
  "inputstate.6" = "Not resident of California",
  "inputstate.8" = "Not resident of Colorado",
  "inputstate.9" = "Connecticut",
  "inputstate.10" = "Delaware",
  "inputstate.11" = "Resident of District of Columbia",
  "inputstate.12" = "Not resident of Florida",
  "inputstate.13" = "Georgia",
  "inputstate.15" = "Hawaii",
  "inputstate.16" = "Idaho",
  "inputstate.17" = "Illinois",
  "inputstate.18" = "Indiana",
  "inputstate.19" = "Not resident of Iowa",
  "inputstate.20" = "Kansas",
  "inputstate.21" = "Kentucky",
  "inputstate.22" = "Louisiana",
  "inputstate.23" = "Not resident of Maine",
  "inputstate.24" = "Maryland",
  "inputstate.25" = "Massachusetts",
  "inputstate.26" = "Michigan",
  "inputstate.27" = "Not resident of Minnesota",
  "inputstate.28" = "Mississippi",
  "inputstate.29" = "Not resident of Missouri",
  "inputstate.30" = "Montana",
  "inputstate.31" = "Nebraska",
  "inputstate.32" = "Nevada",
  "inputstate.33" = "New Hampshire",
  "inputstate.34" = "New Jersey",
  "inputstate.35" = "New Mexico",
  "inputstate.36" = "New York",
  "inputstate.37" = "North Carolina",
  "inputstate.38" = "North Dakota",
  "inputstate.39" = "Not resident of Ohio",
  "inputstate.40" = "Oklahoma",
  "inputstate.41" = "Oregon",
  "inputstate.42" = "Not resident of Pennsylvania",
  "inputstate.44" = "Rhode Island",
  "inputstate.45" = "South Carolina",
  "inputstate.46" = "South Dakota",
  "inputstate.47" = "Tennessee",
  "inputstate.48" = "Texas",
  "inputstate.49" = "Utah",
  "inputstate.50" = "Vermont",
  "inputstate.51" = "Virginia",
  "inputstate.53" = "Washington",
  "inputstate.54" = "West Virginia",
  "inputstate.55" = "Wisconsin",
  "inputstate.56" = "Wyoming",
  "newsint.4" = "Interested in politics: hardly at all",
  "marstat.5" = "Marital status is single",
  "religpew_nondenom.4" = "Nondenominational or Independent Church",
  "votereg.2" = "Not registered to vote",
  "votereg.3" = "Not sure about my voter registration status"
) %>%
  c(vl$common, .)

## Label for CCES 2014 ---------------------------------------------------------
vl[["var14"]] <- c(
  "CC14_302.2" = "Believes national economy has gotten better",
  "CC14_302.3" = "Believes national economy has stayed about the same",
  "CC14_302.4" = "Believes national economy has gotten worse",
  "CC14_302.5" = "Believes national economy has gotten much worse",
  "CC14_308b.2" = "Somewhat approve of US Congress",
  "CC14_310c.4" = "Thinks 3rd party/Independent has majority of seats in Senate 2nd seat",
  "CC14_317.3" = "Voted for 3rd candidate for president",
  "CC14_308a.3" = "Somewhat disapprove of President Obama's job",
  "CC14_308a.4" = "Strongly disapprove of Obama's job",
  "CC14_308b.2" = "Somewhat approves of Congress",
  "CC14_308b.3" = "Somewhat approves of Congress",
  "CC14_308b.4" = "Strongly disapproves of Congress",
  "CC14_317.2" = "Voted for Romney in 2012",
  "CC14_323_1.2" = "Against allowing abortion as a matter of choice",
  "CC14_324_1.2" = "Would not have voted for the Affordable Care Act",
  "CC14_324_2.2" = "Would not vote to repeal the Affordable Care Act",
  "CC14_331_4.2" = "Oppose letting refusal of birth control coverage for religion",
  "CC334C.2" = "Believes Obama is liberal",
  "CC334C.3" = "Believes Obama is somewhat liberal",
  "CC334C.4" = "Believes Obama is middle of the road",
  "CC334C.5" = "Believes Obama is somewhat conservative",
  "CC334C.6" = "Believes Obama is conservative",
  "CC334C.7" = "Believes Obama is very conservative",
  "CC334P.3" = "Believes Senate Candidate 1 is somewhat liberal",
  "CC334P.4" = "Believes Senate Candidate 1 is middle of the road",
  "CC334T.3" = "Believes House Candidate 1 is somewhat liberal",
  "CC334T.4" = "Believes House Candidate 1 is middle of the road",
  "CC334U.5" = "Believes House Candidate 2 is very conservative",
  "CC350.2" = "Was registered to Democratic Party to vote",
  "CC350.3" = "Was registered to Republican Party to vote",
  "CC355.2" = "Prefers Senate Candidate 2",
  "CC355a.2" = "Prefers Senate Candidate 2 as second choice",
  "CC355a.3" = "Prefers Senate Candidate 3 as second choice",
  "CC355a.8" = "Senate candidate second choice: not sure",
  "CC355a.9" = "Senate candidate second choice: no one",
  "CC355a.999" = "(Did not answer Senate candidate second choice)",
  "CC356a.2" = "(First choice for Governor: candidate 2)",
  "CC360.2" = "Prefers House Candidate 2",
  "CC360a.2" = "Prefers House Candidate 2 as second choice",
  "CC360a.3" = "Prefers House Candidate 3 as second choice",
  "CC360a.8" = "House candidate second choice: not sure",
  "CC360a.9" = "House candidate second choice: no one",
  "CC360a.999" = "Not asked/skipped: House candidate second choice",
  "CC411.2" = "Voted for Governor Candidate 2",
  "CC412.2" = "Voted for House Candidate 2",
  "CC412a.2" = "Hypothetically, would vote for House challenger than incumbent (who is retiring)",
  "CC412b.2" = "Hypothetically, would vote for House challenger than incumbent (who is retiring)",
  "CC413a.2" = "Voted Republican in Attorney General",
  "CC413b.2" = "Voted Republican in Secretary of State",
  "CC413c.2" = "Voted Republican in State Senate",
  "CC413d.2" = "Voted Republican in State House",
  "CC421_dem.2" = "I am not so strong Democrat",
  "CC421_dem.999" = "(Not asked whether strong Democrat/not so strong Democrat)",
  "CC421_rep.999" = "(Not asked whether strong Republican/not so strong Republican)",
  "CC421a.2" = "I think of myself as a Republican",
  "CC421b.2" = "I am closer to the Republican party",
  "CC423a.999" = "(Not asked/skipped: race of House candidate 1?)",
  "CC423b.999" = "(Not asked/skipped: race of House candidate 2?)",
  "CC424.2" = "Somewhat positive view of the Tea Party movement",
  "CC424.3" = "Neutral view of the Tea Party movement",
  "CC424.4" = "Negative view of the Tea Party movement",
  "CC424.5" = "Very negative view of the Tea Party movement",
  "CC426_4.5" = "CC426_4.5",
  "impute_1Ljudgevote_post.999" = "impute_1Ljudgevote_post.999",
  "religpew.9" = "Atheist",
  "religpew_protestant.3" = "Prostestant Church: Nondenominational or Independent Church"
) %>%
  c(vl$common, .)

## Label for CCES 2012 ---------------------------------------------------------
vl[["var12"]] <- c(
  "CC302.2" = "Believes national economy has gotten better",
  "CC302.3" = "Believes national economy has stayed about the same",
  "CC302.4" = "Believes national economy has gotten worse",
  "CC302.5" = "Believes national economy has gotten much worse",
  "CC308a.3" = "Somewhat disapprove of way Obama is doing job",
  "CC308a.4" = "Strongly disapprove of way Obama is doing job",
  "CC308b.2" = "Somewhat approves of Congress",
  "CC317.2" = "Voted for McCain in 2008",
  "CC328.2" = "Prefer Congress to cut domestic spending for balanced budget",
  "CC332I.2" = "Oppose Affordable Care Act",
  "CC334J.3" = "Dem. Senate candidate is somewhat liberal",
  "CC334J.4" = "Dem. Senate candidate is middle of the road",
  "CC354b.2" = "Voted Obama for President (early voting)",
  "CC354c.2" = "Prefers Obama for President",
  "CC355b.2" = "Prefers Senate Candidate 2",
  "CC390b.2" = "Prefers House Candidate 2",
  "CC410a.2" = "Voted Romney for President",
  "CC410a_nv.2" = "Prefers Romney for President",
  "CC410b.2" = "Voted Senator 2 for Senate",
  "CC412.2" = "Voted for House Candidate 2",
  "CC412a.2" = "Hypothetically, would vote for House challenger than incumbent (who is retiring)",
  "CC412b.2" = "Hypothetically, would vote for House challenger than incumbent (who is retiring)",
  "CC413a.2" = "Voted Republican in Attorney General",
  "CC413b.2" = "Voted Republican in Secretary of State",
  "CC413c.2" = "Voted Republican in State Senate",
  "CC413d.2" = "Voted Republican in State House",
  "CC421a.2" = "I think of myself as Republican",
  "CC421_dem.999" = "Not asked: would you call yourself strong/not so strong Democrat",
  "CC423a.6" = "Not sure about race of House candidate 1",
  "CC423a.999" = "(Not asked/skipped: race of House candidate 1?)",
  "CC423b.6" = "Not sure about race of House candidate 2",
  "CC423b.999" = "(Not asked/skipped: race of House candidate 2?)",
  "CC424.5" = "Don't know enough about Tea Party to have an opinion"
) %>%
  c(vl$common, .)

## Label for CCES 2010 ---------------------------------------------------------
vl[["var10"]] <- c(
  "cc308a_somewhat_disapprove" = "Somewhat disapprove of Obama's job",
  "cc308a_strongly_disapprove" = "Strongly disapprove of Obama's job",
  "cc317_john_mc_cain_republican" = "Voted McCain for president in 2008",
  "cc328_cut_domestic_spending" = "Prefer Congress to cut domestic spending for balanced budget",
  "cc332a_oppose" = "Oppose American Recovery and Reinvestment Act",
  "cc332d_oppose" = "Oppose Comprehensive Health Reform Act",
  "cc332e_oppose" = "Oppose appointing Elena Kagan to the U.S. Supreme Court",
  "cc334hb_middle_of_the_road" = "Would rate Senator 2 middle of the road",
  "cc350_republican_party" = "Registered with the Republican Party",
  "cc355_sen_cand_name2_republican" = "Prefers Republican candidate for US Senate",
  "cc355b_sen_cand2name2_republican" = "Intend to vote for NY Senate Candidate 2",
  "cc390_house_cand_rep_name_republican" = "Prefers Republican House Candidate",
  "cc410a_sen_cand_name2_republican" = "Voted Republican for US Senate",
  "cc410b_sen_cand_name2_republican" = "Voted Republican for US Senate (Other Race)",
  "cc412_house_cand_rep_name2" = "Voted Republican for US House",
  "cc412_house_cand_rep_name_republican" = "Voted Republican for US House",
  "cc413a_republican_candidate" = "Voted Republican for Attorney General",
  "cc413b_republican_candidate" = "Voted Republican for Secretary of State",
  "cc413c_republican_candidate" = "Voted Republican for State Senate",
  "cc413d_republican_candidate" = "Voted Republican for State House",
  "cc421a_republican" = "Think of myself as Republican",
  "cc421b_the_republican_party" = "Think of myself as closer to the Republican party",
  "cc421_rep_999" = "(Not asked: think of myself as strong/not so strong Republican",
  "v212a_republican" = "Think of myself as Republican"
) %>%
  c(vl$common, .)

## Label for CCES 2008 ---------------------------------------------------------
vl[["var08"]] <- c(
  "CC302.5" = "Believes national economy has gotten much worse",
  "CC304.4" = "Believes US was right in Iraq war/despite mistakes, worth it",
  "CC304.5" = "",
  "CC307.2" = "I think of myself as a Republican",
  "CC307a.7" = "(Before election) I am a strong Republican",
  "CC316a.2" = "Opposes withdrawing troops from Iraq within 180 days",
  "CC317E_BO.2" = "Don't consider Obama experienced",
  "CC317H_BO.2" = "Don't consider Obama honest",
  "CC317H_BO.3" = "Don't know whether Obama is honest",
  "CC317H_HC1.2" = "Don't consider House Dem candidate honest",
  "CC317H_HC1.3" = "Don't know whether House Dem candidate is honest",
  "CC317H_HC2.2" = "Don't consider House Rep candidate honest",
  "CC317H_HC2.3" = "Don't know whether House Rep candidate is honest",
  "CC317H_JM.2" = "Don't consider McCain honest",
  "CC317H_SC1.2" = "Don't consider Senate Dem candidate honest",
  "CC317H_SC1.3" = "Don't know whether Senate Dem candidate is honest",
  "CC317H_SC2.2" = "Don't consider Senate Rep candidate honest",
  "CC317H_SC2.3" = "Don't know whether Senate Rep candidate is honest",
  "CC317I_HC1.2" = "Don't consider House Dem candidate knowledgeable",
  "CC317I_HC1.3" = "Don't know whether House Dem candidate is knowledgeable",
  "CC317I_HC2.2" = "Don't consider House Rep candidate knowledgeable",
  "CC317I_HC2.3" = "Don't know whether House Rep candidate is knowledgeable",
  "CC317I_SC1.2" = "Don't consider Senate Dem candidate knowledgeable",
  "CC323_1.4" = "I would least prefer Obama & Dem Congress",
  "CC323_2.4" = "I would least prefer Obama & Rep Congress",
  "CC323_3.4" = "I would least prefer McCain & Rep Congress",
  "CC323_4.4" = "I would least prefer McCain & Dem Congress",
  "CC327.2" = "Would vote Obama for president",
  "CC335.2" = "Would vote Republican in US Senate",
  "CC335cong.2" = "Somewhat approve of US Congress",
  "CC335bush.4" = "Strongly disapprove of Bush's job",
  "CC339.2" = "Voted Republican for House",
  "CC410.2" = "Voted Obama for president",
  "CC412.2" = "Voted Republican for House",
  "CC414_1.2" = "Voted Republican in State House",
  "CC414_1.6" = "Not sure whom I voted for in State House",
  "CC414_2.2" = "Voted Republican in State Senate",
  "CC423.2" = "Republican",
  "V241.9" = "9 children under 18",
  "V670.2" = "Skipped: Senate other party candidate gender",
  "V673.999" = "Not asked: Senate Republican candidate gender",
  "V675.999" = "Not asked: Senate Republican incumbent gender"
) %>%
  c(vl$common, .)

## Label for CCES 2006 ---------------------------------------------------------
vl[["var06"]] <- c(
  "v2089.4" = "Will definitely vote for Republican candidate in Congressional election",
  "v3003.4" = "Strongly disapprove of Bush's job",
  "v3010.2" = "Do not think it was a mistake to invate Iraq",
  "v3036.2" = "Have not heard of House candidate 1",
  "v3054.2" = "Intend to vote for House candidate 2",
  "v3054.5" = "Haven't decided who to vote for in US House",
  "v3056.2" = "Intend to vote for Senate candidate 2",
  "v3056.5" = "Haven't decided who to vote for in US Senate",
  "v3066.2" = "Against Iraq withdrawal",
  "v3075.2" = "I would vote against capital gains tax",
  "v4013.2" = "Voted Republican for Governor",
  "v4014.2" = "Voted Republican for US Senate",
  "v4015.2" = "Voted Republican for US House",
  "v4020.2" = "Voted Democrat for State House",
  "v4020.3" = "Voted Republican for State House",
  "v4021.2" = "Voted Democrat for State Senate",
  "v4021.3" = "Voted Republican for State Senate",
  "v4034.2" = "Republican",
  "v4036.5" = "(As an Independent) leaning towarsd Republican party",
  "v4037.3" = "(As a Democrat) Democratic party is about right (in ideology)",
  "v4038.5" = "I think Republican party is much too conservative",
  "v4044.2" = "Prefer Congress to cut domestic spending to balance budget",
  "v5002.2" = "House Candidate 1 party: Republican",
  "v5004.999" = "House Candidate 2 party: skipped",
  "v5014.1" = "Sitting House member's party: Democrat",
  "v5014.2" = "Sitting House member's party: Republican",
  "v5023.4" = "Senator 2 actual roll call vote on partial birth abortion measure (options?)"
) %>%
  c(vl$common, .)

## Label for ANES (cumulative) -------------------------------------------------
vl[["anes"]] <- c(
  "vcf0014_1" = "(Pre-election interview data present)",
  "vcf0015a_2" = "Abbreviated Interview: Spanish Pre",
  "vcf0015a_999" = "(Abbreviated Interview: answer missing)",
  "vcf0101" = "Age",
  "vcf0104_2" = "Female",
  "vcf0104_3" = "Gender: Other",
  "vcf0104_999" = "(Gender answer missing)",
  "vcf0105b_2" = "Black",
  "vcf0105b_3" = "Hispanic",
  "vcf0105b_4" = "Asian or Other Race",
  "vcf0105b_999" = "(Race answer missing)",
  "vcf0110_2" = "Education: High School Graduate",
  "vcf0110_3" = "Education: Some College",
  "vcf0110_4" = "Education: College or Higher",
  "vcf0110_999" = "(Education answer missing)",
  "vcf0114_2" = "Income: 17-33 percentile",
  "vcf0114_3" = "Income: 34-67 percentile",
  "vcf0114_4" = "Income: 68-95 percentile",
  "vcf0114_5" = "Income: 96-100 percentile",
  "vcf0114_999" = "(Income answer missing)",
  "vcf0127_2" = "No household member in labor union",
  "vcf0127_999" = "(Household member in labor union: answer missing)",
  "vcf0301_2" = "Weak Democrat",
  "vcf0301_3" = "Independent Democrat",
  "vcf0301_4" = "Independent",
  "vcf0301_5" = "Independent Republican",
  "vcf0301_6" = "Weak Republican",
  "vcf0301_7" = "Strong Republican",
  "vcf0301_999" = "(7-pt Party ID: answer missing)",
  "vcf0343_4" = "President moral: not well at all",
  "vcf0344_3" = "President provides strong leadership: not too well",
  "vcf0348_2" = "President does not make me hopefull",
  "vcf0355_4" = "Dem prez cand moral: not well at all",
  "vcf0360_2" = "Dem Prez Cand: don't feel hopeful",
  "vcf0361_2" = "Dem Prez Cand: don't feel proud",
  "vcf0372_2" = "Rep Prez Cand: don't feel hopeful",
  "vcf0373_2" = "Rep Prez Cand: don't feel proud",
  "vcf0374_5" = "Like anything about Dem party: no",
  "vcf0375d_999" = "(Like anything about Dem party: answer missing)",
  "vcf0388d_999" = "(Like anything about Rep party: answer missing)",
  "vcf0389d_999" = "(Like anything about Dem party: answer missing)",
  "vcf0475_5" = "Something particular contributes to Dem prez cand vote choice: no",
  "vcf0476b_999" = "(Like anything about Dem prez cand: answer missing)",
  "vcf0476d_999" = "(Like anything about Dem prez cand: answer missing)",
  "vcf0477b_999" = "(Like anything about Dem prez cand: answer missing)",
  "vcf0477d_999" = "(Like anything about Dem prez cand: answer missing)",
  "vcf0482d_999" = "(Particular dislike about Dem prez cand: answer missing)",
  "vcf0487_5" = "Like anything about Rep prez cand: no",
  "vcf0488b_999" = "(Like anything about Rep prez cand: answer missing)",
  "vcf0488d_999" = "(Like anything about Rep prez cand: answer missing)",
  "vcf0489b_999" = "(Like anything about Rep prez cand: answer missing)",
  "vcf0489d_999" = "(Like anything about Rep prez cand: answer missing)",
  "vcf0490d_999" = "(Like anything about Rep prez cand: answer missing)",
  "vcf0494b_999" = "(Particular dislike about Rep prez cand: answer missing)",
  "vcf0495b_999" = "(Particular dislike about Rep prez cand: answer missing)",
  "vcf0522_3" = "Party best to avoid (bigger) war: Republicans",
  "vcf0605_2" = "Government is run for: benefit of all",
  "vcf0605_9" = "Government is run for: don't know",
  "vcf0606_2" = "Fed. government waste tax money: some",
  "vcf0606_3" = "Fed. government waste tax money: not very much",
  "vcf0606_9" = "Fed. government waste tax money: don't know",
  "vcf0632_2" = "Branch of govt trusts most: Supreme Court",
  "vcf0632_3" = "Branch of govt trusts most: president",
  "vcf0632_4" = "Branch of govt trusts most: political parties",
  "vcf0632_5" = "Branch of govt trusts most: don't know",
  "vcf0700_2" = "Predict that Rep cand will be elected president",
  "vcf0700_7" = "Predict that third-party cand will be elected president",
  "vcf0707_2" = "Vote for House: Republican",
  "vcf0708_2" = "Vote for Senate: Republican",
  "vcf0712_2" = "I decided to vote this way: when candidate announced",
  "vcf0712_3" = "I decided to vote this way: during conventions",
  "vcf0712_4" = "I decided to vote this way: post-convention period",
  "vcf0712_5" = "I decided to vote this way: last two weeks of campaign",
  "vcf0712_6" = "I decided to vote this way: on Election Day",
  "vcf0712_0" = "(I decided to vote this way: did not vote)",
  "vcf0712_999" = "(I decided to vote this way: answer missing)",
  "vcf0715_2" = "State/local vote: split ticket, mostly Democratic",
  "vcf0715_3" = "State/local vote: split ticket, half and half",
  "vcf0715_4" = "State/local vote: split ticket, mostly Republican",
  "vcf0715_5" = "State/local vote: straight ticket Republican",
  "vcf0716_2" = "I voted straight ticket",
  "vcf0716_9" = "(Straight ticket voting: did not vote)",
  "vcf0716_999" = "(Straight ticket voting: answer missing)",
  "vcf0736_5" = "Vote for House: Republican",
  "vcf0737_2" = "Registered to vote: yes",
  "vcf0737_999" = "(Registered to vote: answer missing)",
  "vcf0805_2" = "Government assistance with medical care: stay out of this",
  "vcf0805_9" = "Government assistance with medical care: not sure",
  "vcf0805_999" = "(Government assistance with medical care: answer missing)",
  "vcf0814_2" = "Civil Rights push: about right",
  "vcf0814_3" = "Civil Rights push: too fast",
  "vcf0814_9" = "Civil Rights push too fast: don't know",
  "vcf0814_999" = "(Civil Rights push too fast: answer missing)",
  "vcf0815_2" = "In favor of sth in-between desegregation and strict segregation",
  "vcf0815_3" = "In favor of strict segregation",
  "vcf0816_2" = "Should govt ensure school integration: stay out of this",
  "vcf0816_9" = "Should govt ensure school integration: don't know",
  "vcf0816_999" = "(Should govt ensure school integration: answer missing)",
  "vcf0818_2" = "Should govt ensure fair jobs/housing for Blacks: agree but not very strongly",
  "vcf0818_3" = "Should govt ensure fair jobs/housing for Blacks: not sure",
  "vcf0818_4" = "Should govt ensure fair jobs/housing for Blacks: disagree but not very strongly",
  "vcf0818_5" = "Should govt ensure fair jobs/housing for Blacks: disagree strongly",
  "vcf0818_999" = "(Should govt ensure fair jobs/housing for Blacks: answer missing)",
  "vcf0822_2" = "Government economic policy: doing only a fair job",
  "vcf0822_3" = "Government economic policy: doing a good job",
  "vcf0822_9" = "Government economic policy doing a good job: don't know",
  "vcf0822_999" = "(Government economic policy doing a good job: answer missing)",
  "vcf0826_2" = "Did US do right thing getting involved in war: yes",
  "vcf0826_9" = "Did US do right thing getting involved in war: depends",
  "vcf0826_999" = "(Did US do right thing getting involved in war: answer missing)",
  "vcf0827_2" = "How should US proceed in current war: try for peace",
  "vcf0827_3" = "How should US proceed in current war: take stronger stand",
  "vcf0827_9" = "How should US proceed in current war: don't know",
  "vcf0827_999" = "(How should US proceed in current war: answer missing)",
  "vcf0828_2" = "Military spending: should continue at least at present level",
  "vcf0828_9" = "Military spending cut: don't know",
  "vcf0828_999" = "(Military spending cut: answer missing)",
  "vcf0829_2" = "Govt in Washington: getting too powerful",
  "vcf0829_9" = "Govt in Washington getting too powerful: don't know",
  "vcf0829_999" = "(Govt in Washington getting too powerful: answer missing)",
  "vcf0833_5" = "Equal Right Amendment: disapprove",
  "vcf0833_8" = "Equal Right Amendment approval: don't know",
  "vcf0833_999" = "(Equal Right Amendment approval: answer missing)",
  "vcf0838_4" = "By law, abortion should be allowed as choice",
  "vcf0852_2" = "Should adjust view of moral behavior: agree somewhat",
  "vcf0852_3" = "Should adjust view of moral behavior: neither agree/disagree",
  "vcf0852_4" = "Should adjust view of moral behavior: disagree somewhat",
  "vcf0852_5" = "Should adjust view of moral behavior: disagree strongly",
  "vcf0852_8" = "Should adjust view of moral behavior: don't know",
  "vcf0852_999" = "(Should adjust view of moral behavior: answer missing)",
  "vcf0867_5" = "Affirmative action in promotion: against",
  "vcf0867_8" = "Affirmative action in promotion approve: don't know",
  "vcf0867_9" = "(Affirmative action in promotion approve: answer missing)",
  "vcf0867a_5" = "Affirmative action in promotion: oppose strongly",
  "vcf0875_2" = "Most important national problem: economics",
  "vcf0875_3" = "Most important national problem: foreign affairs",
  "vcf0875_4" = "Most important national problem: government functioning",
  "vcf0875_5" = "Most important national problem: labor issues",
  "vcf0875_6" = "Most important national problem: natural resources",
  "vcf0875_7" = "Most important national problem: public order",
  "vcf0875_8" = "Most important national problem: racial problems",
  "vcf0875_9" = "Most important national problem: social welfare",
  "vcf0875_97" = "Most important national problem: other problems",
  "vcf0875_999" = "(Most important national problem: answer missing)",
  "vcf0876_5" = "Laws to protect homosexuals against discrimination: oppose",
  "vcf0876_8" = "Laws to protect homosexuals against discrimination favor: don't know",
  "vcf0876_999" = "(Laws to protect homosexuals against discrimination favor: answer missing)",
  "vcf0877_5" = "Homosexuals should be allowed in military: don't think so",
  "vcf0877_8" = "Homosexuals should be allowed in military: don't know",
  "vcf0877_999" = "(Homosexuals should be allowed in military: answer missing)",
  "vcf0877a_2" = "Gays in military: feel not strongly should be allowed",
  "vcf0877a_4" = "Gays in military: feel not strongly should not be allowed",
  "vcf0877a_5" = "Gays in military: feel strongly should not be allowed",
  "vcf0877a_7" = "Gays in military allowed: don't know",
  "vcf0877a_999" = "(Gays in military allowed: answer missing)",
  "vcf0887_2" = "Federal spending on child care: same",
  "vcf0887_3" = "Federal spending on child care: decreased",
  "vcf0887_8" = "Federal spending on child care increased: don't know",
  "vcf0887_999" = "(Federal spending on child care increased: answer missing)",
  "vcf0893_2" = "Federal spending on the homeless: same",
  "vcf0893_3" = "Federal spending on the homeless: decreased",
  "vcf0893_8" = "Federal spending on the homeless increased: don't know",
  "vcf0893_999" = "(Federal spending on the homeless increased: answer missing)",
  "vcf0894_2" = "Federal spending on welfare programs: same",
  "vcf0894_3" = "Federal spending on welfare programs: decreased",
  "vcf0894_8" = "Federal spending on welfare programs increased: don't know",
  "vcf0894_999" = "(Federal spending on welfare programs increased: answer missing)",
  "vcf1004_2" = "Respondent is Republican, House incumbent is Republican",
  "vcf1004_3" = "Respondent is Democrat, House incumbent is Republican",
  "vcf1004_4" = "Respondent is Democrat, House incumbent is Democrat",
  "vcf9009_5" = "President's performance on economy: strongly disapprove",
  "vcf9010_5" = "Republicans would best handle inflation",
  "vcf9012_5" = "Republicans would handle most important problem",
  "vcf9014_2" = "Gone too far pushing equal rights: agree somewhat",
  "vcf9014_3" = "Gone too far pushing equal rights: neither agree/disagree",
  "vcf9014_4" = "Gone too far pushing equal rights: disagree somewhat",
  "vcf9014_5" = "Gone too far pushing equal rights: disagree strongly",
  "vcf9014_8" = "Gone too far pushing equal rights: don't know",
  "vcf9014_999" = "(Gone too far pushing equal rights: answer missing)",
  "vcf9025_2" = "Vote for governor: Republican",
  "vcf9027_2" = "Voted Republican in Last Presidential Election",
  "vcf9027_3" = "Last Presidential Vote: Not Applicable Or Refused",
  "vcf9027_5" = "Voted Third-Party in Last Presidential Election",
  "vcf9027_9" = "Last Presidential Vote: Not Applicable Or Refused",
  "vcf9027_0" = "(Last Presidential Vote: did not vote)",
  "vcf9027_999" = "(Last Presidential Vote: answer missing)",
  "vcf9028_2" = "Believe Rep prez cand will win in respondent's state",
  "vcf9037_5" = "Govt ensure fair jobs for Blacks: not govt's business",
  "vcf9037_9" = "Govt should ensure fair jobs for Blacks: don't know",
  "vcf9037_999" = "(Govt should ensure fair jobs for Blacks: answer missing)",
  "vcf9042_2" = "Blacks gotten less than they deserve: agree somewhat",
  "vcf9042_3" = "Blacks gotten less than they deserve: neither agree nor disagree",
  "vcf9042_4" = "Blacks gotten less than they deserve: disagree somewhat",
  "vcf9042_5" = "Blacks gotten less than they deserve: disagree strongly",
  "vcf9042_8" = "Blacks gotten less than they deserve: don't know",
  "vcf9042_999" = "(Blacks gotten less than they deserve: answer missing)",
  "vcf9044_2" = "Federal govt policy has made economy: somewhat better",
  "vcf9044_3" = "Federal govt policy has made economy: no difference",
  "vcf9044_4" = "Federal govt policy has made economy: somewhat worse",
  "vcf9044_5" = "Federal govt policy has made economy: much worse",
  "vcf9044_6" = "Federal govt policy has made economy: better, don't know how much",
  "vcf9044_7" = "Federal govt policy has made economy: worse, don't know how much",
  "vcf9044_8" = "Federal govt policy has made economy better: don't know",
  "vcf9044_999" = "(Federal govt policy has made economy better: answer missing)",
  "vcf9044a_2" = "Federal govt policy has made economy: same",
  "vcf9044a_3" = "Federal govt policy has made economy: worse",
  "vcf9044a_8" = "Federal govt policy has made economy better: don't know",
  "vcf9045_3" = "Position of US: same in the past year",
  "vcf9045_5" = "Position of US: stronger in the past year",
  "vcf9045_8" = "Position of US stronger in past year: don't know",
  "vcf9045_999" = "(Position of US stronger in past year: answer missing)",
  "vcf9046_2" = "Federal spending on food stamps: same",
  "vcf9046_3" = "Federal spending on food stamps: decreased",
  "vcf9046_7" = "Federal spending on food stamps: cut out entirely",
  "vcf9046_8" = "Federal spending on food stamps increased: don't know",
  "vcf9046_999" = "(Federal spending on food stamps increased: answer missing)",
  "vcf9051_5" = "School prayer: religion does not belong in school",
  "vcf9051_8" = "School prayer should be allowed: don't know",
  "vcf9051_999" = "(School prayer should be allowed: answer missing)",
  "vcf9131_2" = "There are more things govt should be doing",
  "vcf9132_2" = "Free market can handle economy without govt",
  "vcf9132_8" = "Free market can handle economy without govt: don't know",
  "vcf9133_2" = "Govt became bigger because: problems we face are bigger",
  "vcf9133_3" = "Govt became bigger because: don't know",
  "vcf9133_999" = "(Govt became bigger because: answer missing)",
  "vcf9132_999" = "(Free market can handle economy without govt: answer missing)",
  "vcf9204_2" = "Republican party represents respondent's view best",
  "vcf9205_3" = "Republicans would do a better job handling nation's economy",
  "vcf9207_3" = "Somewhat dislike Dem prez cand", ##  (4/11-pt scale)
  "vcf9207_10" = "Strongly like Dem prez cand", ##  (11/11-pt scale)
  "vcf9210_5" = "Dem prez cand does not care about people like me at all",
  "vcf9212_5" = "Honest does not describe Dem prez cand at all",
  "vcf9213_5" = "Strong leadership does not describe Rep prez cand at all",
  "vcf9214_5" = "Rep prez cand does not care about people like me at all",
  "vcf9216_5" = "Honest does not describe Rep prez cand at all",
  "vcf9217_2" = "Way current prez is handling foreign relations: disapprove",
  "vcf9218_2" = "Way current prez is handling health care: disapprove",
  "vcf9222_2" = "The country on right vs. wrong track: wrong track",
  "vcf9236_2" = "Death penalty for murderers: oppose",
  "vcf9237_2" = "Death penalty for murderers: favor not strong",
  "vcf9237_4" = "Death penalty for murderers: oppose not strong",
  "vcf9237_5" = "Death penalty for murderers: oppose strongly",
  "vcf9238_2" = "Should govt make gun sales more difficult: keep same rules",
  "vcf9238_3" = "Should govt make gun sales more difficult: make it easier",
  "vcf9238_999" = "(Should govt make gun sales more difficult: answer missing)",
  "vcf9275_2" = "Blacks have influence on American politics: just about right",
  "vcf9275_3" = "Blacks have influence on American politics: too little",
  "vcf9275_999" = "(Blacks have influence on American politics: answer missing)"
)
