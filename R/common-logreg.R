

library(magrittr)
library(graphics)
library(caret)

#' Uses logistical regression model to make predictions and stores in a dataframe as "class" and "probability"
#' 
#' @param model A trained model object
#' @param testData The evaluation dataset with labels included
#' @param excludeCol The column containing the predictor class labels
#' @param threshold Optional; default 0.5; can tweak threshold for confidence
#' @author Anthogonyst
MakePredictions <- function(model, testData, excludeCol = "target", threshold = 0.5) {
  probability = stats::predict(model, testData[, ! colnames(testData) %in% excludeCol])
  class = probability %>%
    { .[. > threshold] = 1 ; . } %>%
      { .[. <= threshold] = 0 ; . } %>%
        as.factor(.)
  
  data.frame(class, probability)
}

#' Uses logistical regression model to create confusion matrix as a test of validity
#' 
#' @param model A trained model object
#' @param testData The evaluation dataset with labels included
#' @param excludeCol The column containing the predictor class labels
#' @param threshold Optional; default 0.5; can tweak threshold for confidence
#' @author Anthogonyst
VerifyKfold <- function(model, testData, excludeCol = "target", threshold = 0.5) {
  if (inherits(testData, "tbl_df")) {
    actual = data.frame(testData[, colnames(testData) %in% excludeCol])[[excludeCol]]
  } else {
    actual = testData[, colnames(testData) %in% excludeCol]
  }
  
  MakePredictions(model, testData, excludeCol, threshold) %>%
    { table(.$class, actual) } %>%
      ClassificationInfo(.)
}

#' Runs all logistical regression tests
#' 
#' @param model A trained model object
#' @param testData The evaluation dataset with labels included
#' @param excludeCol The column containing the predictor class labels
#' @param threshold Optional; default 0.5; can tweak threshold for confidence
#' @param ... Arguments passed to graphics::fourfoldplot
#' @author Anthogonyst
ValidationPipeline <- function(model, testData, excludeCol, threshold = 0.5, ...) {
  p = MakePredictions(model, testData, excludeCol, threshold)
  d = VerifyKfold(model, testData, excludeCol, threshold)
  g = graphics::fourfoldplot(d, color = c("#B22222", "#2E8B57"), ...)
  
  invisible(list(p, d, g))
}

#' Creates a table to assert differences in model performance
#' 
#' @param testData The evaluation dataset with labels included
#' @param excludeCol The column containing the predictor class labels
#' @param ... Any number of trained model objects
#' @param threshold Optional; default 0.5; can tweak threshold for confidence
#' @author Anthogonyst
CompareModelStats <- function(testData, excludeCol = "target", ..., threshold = 0.5) {
  models = list(...)
  
  lapply(models, VerifyKfold, testData, excludeCol, threshold) %>%
    lapply(., function(x) {
      c(x$byClass["F1"], x$overall["Accuracy"], x$table[1, 1], x$table[1, 2], x$table[2, 1], x$table[2, 2]) %>%
        magrittr::set_names(., c("F1", "Accuracy", "TP", "FP", "FN", "TN"))
    }) %>%
      do.call(rbind, .)
}


##### Classification #####

#' @author Anthogonyst
.SumOf <- function(df, FUN, ...) {
  sum(df[FUN(df, ...)])
}

#' Creates an annotated table similar to caret::confusionMatrix
#' @author Anthogonyst
ClassificationInfo <- function(df, applyTable = FALSE, verbose = FALSE) {
  if (applyTable) {
    df = table(df)
  }
  
  trueNeg = df[1, 1]
  falsePos = .SumOf(df, upper.tri)
  falseNeg = .SumOf(df, lower.tri)
  truePos = .SumOf(df, .diag, TRUE)
  
  ### Calculates statistical relevance from the data frame
  accuracy = .SumOf(df, .diag) / sum(df)
  errors = 1 - accuracy
  precision = .SumOf(df, .diag, TRUE) / (.SumOf(df, .diag, TRUE) + .SumOf(df, upper.tri))
  sensitivity = .SumOf(df, .diag, TRUE) / (.SumOf(df, .diag, TRUE) + .SumOf(df, lower.tri))
  specificity = df[1, 1] / (df[1, 1] + .SumOf(df, upper.tri))
  f1 = 2 * precision * sensitivity / (precision + sensitivity)
  
  attr(df, "accuracy") = accuracy
  attr(df, "error.rate") = errors
  attr(df, "precision") = precision
  attr(df, "sensitivity") = sensitivity
  attr(df, "specificity") = specificity
  attr(df, "f1") = f1
  
  if (verbose) {
    message("   Accuracy: ", round(accuracy, 4))
    message(" Error Rate: ", round(errors, 4))
    message("  Precision: ", round(precision, 4))
    message("Sensitivity: ", round(sensitivity, 4))
    message("Specificity: ", round(specificity, 4))
    message("   F1 Score: ", round(f1, 4))
  }
  
  return(df)
}


##### Receiver Operator Curve #####

#' @author Anthogonyst
Roc <- function(target, probability, points = 20) {
  threshold = seq(1, 0, length.out = points)
  
  newPredictions = sapply(threshold, function(x) {
    df = table(target, ifelse(probability > x, 1, 0))
    
    sensitivity = .SumOf(df, .diag, TRUE) / (.SumOf(df, .diag, TRUE) + .SumOf(df, lower.tri))
    specificity = df[1, 1] / (df[1, 1] + .SumOf(df, upper.tri))
    
    if (dim(df)[1] > dim(df)[2]) {
      if (x >= 0.5) {
        sensitivity = 0
        specificity = 1
      } else {
        sensitivity = 1
        specificity = 0
      }
    }
    
    c(x = 1 - specificity, y = sensitivity, t = x)
  })
  
  roc = data.frame(t(newPredictions))
  attr(roc, "AUC") = TrapezoidArea(roc$x, roc$y)
  
  return(roc[! duplicated(roc[, c(1, 2)]), ])
}

#' @author Anthogonyst
.PlotRocMany <- function(roc, lineFUN = ggplot2::geom_step) {
  ### Annotates the values of every point
  roc["prettyLabel"] = paste0(as.character(round(100 * roc[["t"]], 1)), "%")
  roc["prettyColors"] = grDevices::colorRampPalette(
    RColorBrewer::brewer.pal(3, "Spectral")
  )(nrow(roc))[roc$t*5+5]
  
  ### Drops all labels except the first of every group
  roc[duplicated(roc["prettyColors"]), "prettyLabel"] = ""
  
  ggplot2::ggplot(roc) +
    ggplot2::aes(x = x, y = y, color = prettyColors, label = prettyLabel) +
    lineFUN(size = 1.5) +
    ggplot2::geom_text(hjust = -0.1, vjust = 1.5) +
    ggplot2::geom_abline(slope = 1, color = "#5570ff") +
    ggplot2::xlab("False Positives") +
    ggplot2::ylab("Hit Rate") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

#' @author Anthogonyst
.PlotRocFew <- function(roc, lineFUN = ggplot2::geom_step) {
  ### Annotates the values of every point
  roc["prettyLabel"] = paste0(as.character(round(100 * roc[["t"]], 1)), "%")
  
  ggplot2::ggplot(roc) +
    ggplot2::aes(x = x, y = y, label = prettyLabel) +
    lineFUN(size = 1.5) +
    ggplot2::geom_text(hjust = -0.1, vjust = 1.5) +
    ggplot2::geom_abline(slope = 1, color = "#5570ff") +
    ggplot2::xlab("False Positives") +
    ggplot2::ylab("Hit Rate") +
    ggplot2::theme_minimal()
}

#' @author Anthogonyst
PlotRoc <- function(roc, lineFUN = ggplot2::geom_step) {
  if (nrow(roc) > 30) {
    .PlotRocMany(roc, lineFUN)
  } else {
    .PlotRocFew(roc, lineFUN)
  }
}

