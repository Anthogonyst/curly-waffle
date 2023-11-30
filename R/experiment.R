
ExperimentOne <- function(df) {
  CleanColsAssessment(df)
}

ExperimentTwo <- function(df) {
  finance = LoadMarketData()
  RemoveCols(df, colnames(finance))
}

ExperimentThree <- function(df) {
  finance = LoadMarketData()
  RemoveCols(df, invert = TRUE,
    c("y", colnames(finance), "lotarea", "bldgarea", "numfloors", "numbldgs", "yearbuilt")
  )
}

ExperimentFour <- function(df) {
  df %>%
    CleanInfluencers(.) %>%
      dplyr::filter(., factryarea < 1, retailarea < 1) %>%
        dplyr::select(., -factryarea, -retailarea) %>%
          RemoveInfluencers(., lm(y~., .), 4) %>%
            RemoveInfluencers(., lm(y~., .), 4)
}

ExperimentFive <- function(df) {
  familyHomes = dplyr::filter(df, landuse == 1)
  familyHomes$landuse = NULL
  
  familyHomes
}

# Exclude y
PreProcessExp1 <- function(df) {
  df$y = NULL
  caret::preProcess(df, method = c("scale", "center", "YeoJohnson"))
}

# Exclude y/many
PreProcessExp2 <- function(df) {
  df$y = NULL
  df$yearbuilt = NULL
  df$yearalter1 = NULL
  df$yearalter2 = NULL
  df$saledate = NULL
  df$CPIAUCSL = NULL
  df$GDP = NULL
  df$FEDFUNDS = NULL
  df$UNRATE = NULL
  df$CSUSHPISA = NULL
  df$EFFR = NULL
  df$PPIACO = NULL
  
  caret::preProcess(df, method = c("scale", "center"))
}

### Exclude many
PreProcessExp3 <- function(df) {
  df$yearbuilt = NULL
  df$yearalter1 = NULL
  df$yearalter2 = NULL
  df$saledate = NULL
  df$CPIAUCSL = NULL
  df$GDP = NULL
  df$FEDFUNDS = NULL
  df$UNRATE = NULL
  df$CSUSHPISA = NULL
  df$EFFR = NULL
  df$PPIACO = NULL
  
  caret::preProcess(df, method = c("scale", "center"))
}

# YeoJohnson and exclude y/many
PreProcessExp4 <- function(df) {
  df$y = NULL
  df$yearbuilt = NULL
  df$yearalter1 = NULL
  df$yearalter2 = NULL
  df$saledate = NULL
  df$CPIAUCSL = NULL
  df$GDP = NULL
  df$FEDFUNDS = NULL
  df$UNRATE = NULL
  df$CSUSHPISA = NULL
  df$EFFR = NULL
  df$PPIACO = NULL
  
  caret::preProcess(df, method = c("scale", "center", "YeoJohnson"))
}

### YeoJohnson and exclude many
PreProcessExp5 <- function(df) {
  df$yearbuilt = NULL
  df$yearalter1 = NULL
  df$yearalter2 = NULL
  df$saledate = NULL
  df$CPIAUCSL = NULL
  df$GDP = NULL
  df$FEDFUNDS = NULL
  df$UNRATE = NULL
  df$CSUSHPISA = NULL
  df$EFFR = NULL
  df$PPIACO = NULL
  
  caret::preProcess(df, method = c("scale", "center", "YeoJohnson"))
}

PreProcessApply <- function(df, tf) {
  caret:::predict.preProcess(tf, newdata = df)
}

UndoPreProcess <- function(df, previous, pattern = "^y_?\\d?.?$") {
  if (any(colnames(df) != colnames(previous))) {
    stop("Two data frames are not identical in columns.")
  }
  
  ind = which(grepl(pattern, colnames(df)))
  
  df[, ind] = previous[, ind]
  df
}
