library(MASS) ## otherwise, overwrites dplyr::select

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
file_path_fxn <- function(data = "CCES", pid = FALSE) {
  if (pid) {
    here(
      "output", data,
      method, paste0(method, "_", yr, "_st", varset, "_pid.Rda")
    )
  } else {
    here(
      "output", data,
      method, paste0(method, "_", yr, "_st", varset, ".Rda")
    )
  }
}

one_hot <- function(df) {
  x <- predict(caret::dummyVars(~., df, fullRank = TRUE), df)
  output <- as_tibble(x)
  return(output)
}

stata_varlabel_df <- function(x) {
  x %>%
    map(~ attr(.x, "label")) %>%
    map(~ tibble(label = .x)) %>%
    map_df(rownames_to_column, .id = "var") %>%
    select(-rowname)
}

data_routine <- function(df, dep, lvl, lbl, dbl = NULL, na = 999, seed = 100,
                         max_factors = 20, freq_cut = 99) {
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
    map(~ (length(unique(.x)) < 2 | length(unique(.x)) > max_factors)) %>%
    unlist() %>%
    which()
  if (length(setdiff(temp, match(dbl, names(df), nomatch = 0))) > 0) {
    df <- df[, -setdiff(temp, match(dbl, names(df), nomatch = 0))] %>%
      ## If missing continuous values, delete row
      filter_at(dbl, all_vars(!is.na(.)))
  }

  ## Near-zero variance variables
  temp <- caret::nearZeroVar(df, freqCut = freq_cut / (100 - freq_cut))
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
    mutate(across(where(is.integer), as.factor)) %>%
    mutate(across(where(is.numeric), as.factor)) %>%
    mutate(
      across(
        tidyselect::all_of(intersect(dbl, names(df))),
        ~ as.numeric(as.character(.x))
      )
    )

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
    if (length(unique(temp$train$depvar)) == 2) {
      out <- train(
        as.factor(depvar) ~ .,
        metric = metric,
        method = "glm",
        family = "binomial",
        trControl = tc,
        data = temp$train
      )
    } else {
      out <- train(
        as.factor(depvar) ~ .,
        metric = metric,
        method = "multinom",
        trControl = tc,
        data = temp$train
      )
    }
  } else if (method == "ol") {
    ## https://github.com/topepo/caret/blob/master/RegressionTests/Code/polr.R
    mod <- polr(as.factor(depvar) ~ ., data = temp$train)
    strt <- c(coef(mod), mod$zeta)
    xdat <- temp$train %>%
      ## no variation variables dropped
      select(-setdiff(names(temp$train), c(names(strt), "depvar")))

    ## Error in optim(s0, fmin, gmin, method = "BFGS", ...) :
    ## initial value in 'vmmin' is not finite
    out <- NULL
    tryCatch(
      {
        out <- train(
          as.factor(depvar) ~ .,
          metric = metric,
          method = "polr",
          trControl = tc,
          data = xdat,
          start = strt
        )
      },
      error = function(e) {
        message(e)
      }
    )
    if (is.null(out)) {
      out <- train(
        as.factor(depvar) ~ .,
        metric = metric,
        method = "polr",
        trControl = tc,
        data = xdat
      )
    }
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
    message("Performance metrics for year ", yr, ": ", output$auc)
  }

  return(output)
}

perf_summ <- function(perf, method, set, yr = rev(seq(2006, 2018, 2))) {
  data.frame(
    Year = yr,
    AUC = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$auc) %>% unlist(),
    AUC_lower = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$ci.auc.bootstrap) %>% map(1) %>% unlist(),
    ## map(2) ---> AUC
    AUC_upper = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$ci.auc.bootstrap) %>% map(3) %>% unlist(),
    Accuracy = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$cf.matrix$overall[["Accuracy"]]) %>%
      unlist(),
    Accuracy_lower = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$cf.matrix$overall[["AccuracyLower"]]) %>%
      unlist(),
    Accuracy_upper = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$cf.matrix$overall[["AccuracyUpper"]]) %>%
      unlist(),
    CI = perf %>% map(method) %>% map(paste0("set", set)) %>%
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
    Precision = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$cf.matrix$byClass[c("Precision")]) %>%
      map(~ round(.x, digits = 4)) %>%
      unlist(),
    Recall = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$cf.matrix$byClass[c("Recall")]) %>%
      map(~ round(.x, digits = 4)) %>%
      unlist(),
    F1 = perf %>% map(method) %>% map(paste0("set", set)) %>%
      map(~ .x$cf.matrix$byClass[c("F1")]) %>%
      map(~ round(.x, digits = 4)) %>%
      unlist(),
    row.names = NULL
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
                    colour_nrow = 2, linetype_nrow = 2, end = 0.9, vdir = -1,
                    name = "Specification", accrange = FALSE, y2 = FALSE,
                    breaks = seq(1952, 2020, by = 4)) {
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

  ## If two y-variables, as in edited manuscript
  if (y2) {
    p <- ggplot(
      x, aes(x = Year, y = !!as.name(metric), colour = Set, shape = Set)
    )
  }

  if (accrange) {
    p <- p + geom_pointrange(aes(ymin = Accuracy_lower, ymax = Accuracy_upper))
  } else {
    p <- p +
      geom_line(size = 1)
  }

  p <- p +
    geom_point(aes(shape = Set)) +
    scale_shape_discrete(name = name) +
    scale_x_continuous(breaks = breaks) +
    scale_color_viridis_d(direction = vdir, name = name, end = end) +
    guides(
      colour = guide_legend(nrow = colour_nrow, byrow = TRUE),
      shape = guide_legend(nrow = colour_nrow, byrow = TRUE)
    )

  if (length(unique(x$Survey)) > 1) {
    lty <- c("solid", "dashed", "dotdash", "dotted")

    p <- p +
      scale_linetype_manual(
        name = "Survey", values = lty[seq(length(unique(x$Survey)))]
      ) +
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

roc_comparison <- function(perf, set = 4,
                           levels = c("logit", "cart", "rf"),
                           labels = c("Logit", "CART", "RF"),
                           linetype = c("dotdash", "dashed", "solid"),
                           size = 0.7,
                           position = c(0.8, 0.2)) {
  temp <- levels %>%
    imap_dfr(
      function(x, y) {
        perf %>%
          imap(x) %>%
          imap(paste0("set", set)) %>%
          imap(~ .x$perf) %>%
          imap_dfr(
            ~ tibble(method = x, x = .x@x.values[[1]], y = .x@y.values[[1]]),
            .id = "year"
          )
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
width <- 6.2
height <- 4

options(
  mc.cores = parallel::detectCores(),
  java.parameters = "-Xmx32g",
  digits = 4,
  scipen = 100
)

set_labels <- c(
  "Demographics Only", "Demo. + PID", "Demo. + PID + Issues", "All Covariates",
  ## Appendix requested
  "Demo. + Religion",
  "Demo. + South",
  "Demo. Only, Broader Definition",
  "Demo. + Ideology",
  "Demo. + Issues"
)

anes_years <- seq(1952, 2020, by = 4)
cces_years <- seq(2008, 2018, by = 2)
anes_sets <- seq(9)

pid_labels <- c(
  "strong_democrat", "weak_democrat", "independent_democrat", "independent",
  "independent_republican", "weak_republican", "strong_republican"
)

anes_sets_pid <- seq(10, 15)
set_labels_pid <- c(
  "7-pt PID, Demographics Only", "7-pt PID, Demo. Only, Broader Definition",
  "3-pt PID, Demographics Only", "3-pt PID, Demo. Only, Broader Definition",
  "Binary PID, Demographics Only", "Binary PID, Demo. Only, Broader Definition"
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

methods <- c("logit", "cart", "rf") %>% set_names(., .)
