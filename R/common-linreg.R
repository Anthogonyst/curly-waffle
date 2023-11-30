

library(ggplot2)

##### Linear Regression Assumptions #####

#' One of three plots necessary to verify assumptions of linear regression
#' @author Anthogonyst
PlotLinearity <- function(lm_calc, col = "#FF464A", extend = NULL) {
  ggplot2::ggplot(data = lm_calc) +
    ggplot2::aes(x = lm_calc$fitted.values, y = lm_calc$residuals) +
    ggplot2::geom_jitter(width = 0.01, color = col) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::xlab("Fitted values") +
    ggplot2::ylab("Residuals") +
    ggplot2::theme(legend.position = "none") +
    extend
}

#' One of three plots necessary to verify assumptions of linear regression
#' @author Anthogonyst
PlotDistribution <- function(lm_calc, col = "#FF464A", extend = NULL) {
  ggplot2::ggplot(data = lm_calc) +
    ggplot2::aes(x = lm_calc$residuals, y = "") + 
    ggplot2::geom_violin(adjust = 1L, scale = "area", fill = col, show.legend = FALSE) +
    ggplot2::xlab("Residuals") +
    ggplot2::ylab("Distribution") +
    extend
}

#' One of three plots necessary to verify assumptions of linear regression
#' @author Anthogonyst
PlotQQuantile <- function(lm_calc, col = "#FF464A", extend = NULL) {
  ggplot2::ggplot(data = lm_calc) +
    ggplot2::aes(sample = lm_calc$residuals) +
    ggplot2::stat_qq(color = col) +
    ggplot2::theme(legend.position = "none") +
    extend
}

#' Calculates outliers by Cook's Distance
#' @author Anthogonyst
Influencers <- function(lm_calc, sd = 3) {
  # Grabbed from stats::plot.lm
  cookhat <- (influencers <- influence(lm_calc, do.coef = FALSE))$hat
  cooks = cooks.distance(lm_calc, influencers)
  leverage = cooks[cooks > (sd * mean(cooks, na.rm = TRUE))]
  leverage
}

#' Removes outliers by Cook's Distance for a given dataframe and linear regression model
#' @author Anthogonyst
RemoveInfluencers <- function(df, lm_calc, sd = 3, returnModel = FALSE) {
  infl = Influencers(lm_calc, sd)
  dat = dplyr::anti_join(df, df[names(infl), ], by = attr(lm_calc$terms, "term.labels"))
  
  if (returnModel) {
    return(update(lm_calc, data = dat))
  }
  
  dat
}

LinearModelStatistics <- function(lm_calc, testData) {
  if (is.null(colnames(testData))) {
    y = NULL
    if (length(testData) > 0) {
      warning("You need to pass a dataframe to generate predictions.")
    }
  } else if ("y" %in% colnames(testData)) {
    y = testData$y
  } else {
    y = testData[[1]]
  }
  
  stats = lm_calc %>%
    lapply(., summary) %>%
      { data.frame(
        Model = paste("Model", seq_len(length(lm_calc))),
        `R^2` = sapply(., with, r.squared),
        `R^2 Adj` = sapply(., with, adj.r.squared),
        `F Value` = sapply(., with, fstatistic[1]),
        `F (NumDF)` = sapply(., with, fstatistic[2]),
        `F (DenDF)` = sapply(., with, fstatistic[3])
      )}
  
  if (! is.null(y)) {
    metrics = lm_calc %>%
      lapply(predict.lm, testData) %>%
        { data.frame(
          Model = paste("Model", seq_len(length(lm_calc))),
          RMSE = sapply(., ModelMetrics::rmse, y),
          MSE = sapply(., ModelMetrics::mse, y),
          MAE = sapply(., ModelMetrics::mae, y),
          AIC = sapply(lm_calc, stats::AIC)
        )}
    
    cbind(stats, metrics)
  } else {
    stats
  }
}
