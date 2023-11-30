

library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(grDevices)

##### Distribution #####

#' Plots the distribution of variables in a dataset
#' The visualization is a violin plot across facets with optional boxplot and groupings
#'
#' @param df The dataframe
#' @param targetCol Optional; grouping column
#' @param showBoxPlot Default true; adds a small boxplot for quantiles
#' @param extend Optional; default null; a ggproto object to append to the visualization
#' 
#' @examples
#' \dontrun{
#'
#' data(iris)
#' PlotVarDistribution(iris, "Species", showBoxPlot = FALSE)
#' 
#' data(Sacramento)
#' PlotVarDistribution(Sacramento)
#' }
#' @author Anthogonyst
PlotVarDistribution <- function(df, targetCol = NULL, showBoxPlot = TRUE, extend = NULL) {
  
  meltDf = reshape2::melt(df[, sapply(df, is.numeric) | colnames(df) %in% targetCol], 
                          id = targetCol, value.name = "value") 
  noGroupColor = if (is.null(targetCol)) { "#FF464A" } else { NULL }
  
  # TODO: geom_violin(fill = NULL) crashes
  # TODO: boxplots over many graphs puts them in wrong position too close to center
  # When above two bugs are fixed, we may delete below line
  showBoxPlot = if (is.null(targetCol)) { showBoxPlot } else { FALSE }
  
  if (showBoxPlot) {
    return(
      ggplot2::ggplot(meltDf[! is.na(meltDf["value"]), ]) +
        ggplot2::aes(x = variable, y = value) +
        ggplot2::aes_string(group = targetCol) +
        ggplot2::geom_violin(adjust = 1L, scale = "area", fill = noGroupColor) +
        ggplot2::geom_boxplot(width = 0.1) +
        extend +
        ggplot2::theme_minimal() +
        ggplot2::facet_wrap(~variable, scales = "free")
    )
  } else {
    return(
      ggplot2::ggplot(meltDf[! is.na(meltDf["value"]), ]) +
        ggplot2::aes(x = variable, y = value) +
        ggplot2::aes_string(group = targetCol, fill = targetCol) +
        ggplot2::geom_violin(adjust = 1L, scale = "area") +
        extend +
        ggplot2::theme_minimal() +
        ggplot2::facet_wrap(~variable, scales = "free")
    )
  }
}


##### Correlation #####

#' Creates a visualization of a correlation matrix
#' @author Anthogonyst
PlotCorrEllipse <- function(corData, pal = RColorBrewer::brewer.pal(5, "Spectral"),
                            highlight = c("both", "positive", "negative")[1], hiMod = 1) {
  colorRange = 100
  coloredVals = corData*50 + 50
  skewRange = max(min(hiMod[[1]], 2), 0)
  
  if (is.numeric(pal)) {
    warning("Passed number as argument for palette. It works but was this intentional?")
  }
  if (skewRange != hiMod) {
    warning("Color range skew only allowed between 0 and 2 where 0.5*n% of value range are colored.  cor(X) C [-1, 1]")
  }
  if (highlight[[1]] == "positive") {
    colorRange = 50 * skewRange
    coloredVals = 1 - corData*50 + 50
  }
  if (highlight[[1]] == "negative") {
    colorRange = 50 * skewRange
    coloredVals = corData*50 + 50
  }
  
  ellipse::plotcorr(corData, mar = c(1,1,1,1),
                    col = grDevices::colorRampPalette(pal)(colorRange)[coloredVals])  
}


CorrelationFunnel <- function(df, target = "y", bins = 4, 
                              mode = c("positive", "neutral", "negative", "all")[[1]], showPlot = TRUE) {
  positive = paste0("^", target, ".*[^-]Inf$")
  negative = paste0("^", target, ".*-Inf")
  
  cf = df %>%
    correlationfunnel::binarize(., n_bins = bins) %>%
      correlationfunnel::correlate(., target = grep(positive, colnames(.), value = TRUE)[[1]])

  if (showPlot[[1]]) {
    return(correlationfunnel::plot_correlation_funnel(cf) +
           ggplot2::ggtitle(paste0("Correlation Distance (", mode, ")")))
  }
  
  return(cf)
}

.CorrelationFunnelNeutral <- function(df, target = "y", mode = NULL, showPlot = TRUE) { }
.CorrelationFunnelBinned <- function(df, target = "y", mode = c("positive", "negative")[[1]], showPlot = TRUE) { }
.CorrelationFunnelAll <- function(df, target = "y", mode = NULL, showPlot = TRUE) { }

##### Descriptive Statistics #####

#' @author Anthogonyst
PlotMissingData <- function(df, sortBy = c("default", "reverse", "original")[1]) {
  if (sortBy[[1]] == "reverse") {
    ordering = xtfrm(colnames(df))
  } else if (sortBy[[1]] == "original") {
    ordering = -seq_along(colnames(df))
  } else {
    ordering = -xtfrm(colnames(df))
  }
  
  mdf = data.frame(colx = colnames(df), missy = colSums(is.na(df)), ord = ordering)
  mx = max(mdf$missy)
  sm = nrow(df)
  
  ggplot2::ggplot(data = mdf) +
  ggplot2::aes(x = stats::reorder(colx, ord), y = missy, fill = colx) +
  ggplot2::geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  ggplot2::geom_text(colour = "white", size = 2.5,
    ggplot2::aes(y = 0.5 * missy, label = paste0(
      ifelse(missy > mx * .08, missy, ""),
      ifelse(missy > mx * .21, paste0(" (", sprintf("%1.1f", 100*missy/sm), "%)"), "")
    ))
  ) +
  ggplot2::scale_colour_gradientn(colours = grDevices::rainbow(36)) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "", y = "Missing Values")
}


