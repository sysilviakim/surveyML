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
  here(
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
  } else if (method == "logit") {
    out <- train(
      as.factor(depvar) ~ .,
      metric = metric,
      method = "glm",
      family = "binomial",
      trControl = tc,
      data = temp$train
    )
  }
  return(out)
}

pdf_varimp <- function(x, filename, labels = NULL, font = NULL, size = 12) {
  if (is.null(font)) font <- "Kievit Offc Pro"
  set.seed(100)
  temp <- varImp(x)
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

po_full <- function(x, metric, ylim = c(0.38, 1.0),
                    colour_nrow = 2, linetype_nrow = 2, end = 0.9, vdir = -1) {
  if (length(unique(x$Survey)) > 1) {
    p <- ggplot(
      x,
      aes(
        x = Year, y = !!as.name(metric),
        colour = Set, linetype = Survey, shape = Set
      )
    )
  } else {
    p <- ggplot(
      x, aes(x = Year, y = !!as.name(metric), colour = Set, shape = Set)
    )
  }

  p <- p +
    geom_line(size = 1) + 
    geom_point(aes(shape = Set)) +
    scale_shape_discrete(name = "Specification") +
    scale_x_continuous(breaks = c(anes_years, 2020)) +
    scale_color_viridis_d(direction = vdir, name = "Specification", end = end) +
    guides(
      colour = guide_legend(nrow = colour_nrow, byrow = TRUE),
      shape = guide_legend(nrow = colour_nrow, byrow = TRUE)
    )

  if (length(unique(x$Survey)) > 1) {
    p <- p +
      scale_linetype_manual(name = "Survey", values = c("solid", "dashed")) +
      guides(
        colour = guide_legend(nrow = colour_nrow, byrow = TRUE),
        shape = guide_legend(nrow = colour_nrow, byrow = TRUE),
        linetype = guide_legend(nrow = linetype_nrow, byrow = TRUE)
      )
  }

  p <- p +
    scale_y_continuous(limits = ylim)
  return(p)
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

roc_comparison <- function(perf, yvar = "prezvote", set = 4,
                           levels = c("logit", "cart", "rf"),
                           labels = c("Logit", "CART", "RF"),
                           linetype = c("dotdash", "dashed", "solid"),
                           size = 0.7,
                           position = c(0.8, 0.2)) {
  temp <- levels %>%
    map_dfr(
      function(x, y) {
        perf %>%
          imap(yvar) %>%
          imap(x) %>%
          imap(set) %>%
          imap(~ .x$perf) %>%
          imap_dfr(
            ~ tibble(method = x, x = .x@x.values[[1]], y = .x@y.values[[1]]),
            .id = "year"
          ) %>%
          mutate(yvar = yvar)
      }
    ) %>%
    mutate(method = factor(method, levels = levels, labels = labels))

  p <- unique(temp$year) %>%
    set_names(., .) %>%
    imap(
      ~ ggplot(
        temp %>% filter(year == .x),
        aes(x = x, y = y, colour = method, linetype = method)
      ) +
        geom_line(size = size) +
        scale_color_viridis_d(direction = -1, end = 0.9) +
        scale_linetype_manual(values = linetype) +
        xlab("False Positive Rate") +
        ylab("True Positive Rate") +
        theme(legend.position = position) +
        labs(colour = "Method", linetype = "Method")
    )
  return(p)
}

## Extra Setup =================================================================
options(
  mc.cores = parallel::detectCores(),
  java.parameters = "-Xmx32g",
  digits = 4,
  scipen = 100
)

set_labels <- c(
  "Demographics Only", "Demo. + PID", "Demo. + PID + Issues", "All Covariates",
  ## Appendix requested
  paste0("Demo. + ", c("Religion", "South", "Ideology", "Issues"))
)
anes_years <- seq(1952, 2016, by = 4)
cces_years <- seq(2008, 2018, by = 2)

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
