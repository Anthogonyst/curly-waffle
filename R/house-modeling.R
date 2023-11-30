
.Exclude <- function(df, column, FUN, ...) {
  exclude = which(FUN(df[[column]], ...))
  
  if (length(exclude) > 0) {
    df[-exclude, ]
  } else {
    df
  }
}

.FilterTaxClass <- function(df, taxClass = c("1", "1B", "2", "2A", "2B", "2C", "3", "4")) {
  .Exclude(df, "TAX CLASS AT PRESENT", function(x, ...) { ! x %in% ... }, taxClass)
}

.OneUnitOnly <- function(df) {
  .Exclude(df, "TOTAL UNITS", function(x, ...) { x != 1 })
}

.OneEasementOnly <- function(df) {
  .Exclude(df, "easements", function(x, ...) { x != 1 })
}

.OneBuildingOnly <- function(df) {
  .Exclude(df, "numbldgs", function(x, ...) { x != 1 })
}

.ReduceBuildingClass <- function(df) {
  results = df[["BUILDING CLASS CATEGORY"]] %>%
    { ifelse(
      grepl("FAMILY HOMES", .),
      gsub(".*\\W(\\w+) FAMILY HOMES.*", "\\1", .),
      gsub(".*", "OTHER", .)
    )}
  
  if (length(results) > 0) {
    df[["BUILDING CLASS CATEGORY"]] = results
    attr(df, "building.class.mod") = TRUE
  }
  
  df
}

.SimplifyBuildingClass <- function(df) {
  results = df[["landuse"]] %>%
    grepl("01", .)

  if (length(results) > 0) {
    df[["landuse"]] = results
    attr(df, "land.use.mod") = TRUE
  }
  
  df
}

TargetPrice <- function(df) {
  dplyr::rename(df, y = `SALE PRICE`)
}

TargetTaxPrevious <- function(df) {
  dplyr::rename(df, y = `TAX CLASS AT TIME OF SALE`)
}

TargetTaxToday <- function(df) {
  dplyr::rename(df, y = `TAX CLASS AT PRESENT`)
}

CleanCols <- function(df, TargetFUN = TargetPrice) {
  pluto = LoadPluto(n_max = 3)
  fin = LoadMarketData()
  expectedTargets = c("SALE PRICE", "TAX CLASS AT PRESENT", "TAX CLASS AT TIME OF SALE")
  
  df = RemoveCols(df, c("y", "SALE DATE", expectedTargets, colnames(pluto), colnames(fin)), invert = TRUE)
  
  excludeCols = c(
    "cd", "bct2020", "bctcb2020", "ct2010", "cb2010", "council", "firecomp",
    "healtharea", "sanitboro", "sanitdistrict", "sanitsub", "address", "borocode",
    "zonedist1", "bldgclass", "areasource", "proxcode", "bbl", "tract2010",
    "zonemap", "sanborn", "taxmap", "plutomapid", "version", "borough", "ownername"
  )
  
  RemoveCols(df, excludeCols) %>%
    TargetFUN(.) %>%
      RemoveCols(., c("y", "SALE DATE", colnames(pluto), colnames(fin)), invert = TRUE) %>%
        # Used mutate instead of rename to disrupt order of column from n to last
        dplyr::mutate(saledate = `SALE DATE`) %>%
          dplyr::select(-`SALE DATE`)
}

CleanCoordinates <- function(df) {
  RemoveCols(df, c("xcoord", "ycoord", "latitude", "longitude"))
}

CleanColsStricter <- function(df, TargetFUN = TargetPrice) {
  excludeCols = c(
    "borough", "zipcode", "policeprct", "healthcenterdistrict", "splitzone",
    "landuse", "lottype", "yearalter1", "yearalter2"
  )
  
  RemoveCols(CleanCols(df, TargetFUN), excludeCols)
}

CleanColsRatio <- function(df) {
  excludeCols = c(
    "unitstotal", "lotfront", "lotdepth", "bldgfront", "bldgdepth",
    "irrlotcode", "lottype", "builtfar", "residfar", "commfar", "facilfar"
  )
  
  RemoveCols(df, excludeCols)
}

CleanColsAssessment <- function(df) {
  RemoveCols(df, c("assessland", "assesstot", "exempttot"))
}

CleanFinance <- function(df) {
  fin = LoadMarketData()
  RemoveCols(df, colnames(fin))
}

CleanInfluencers <- function(df) {
  df %>%
    RemoveInfluencers(., lm(y~., .), 1) %>%
      RemoveInfluencers(., lm(y~., .), 1) %>%
        RemoveInfluencers(., lm(y~., .), 1) %>%
          RemoveInfluencers(., lm(y~., .), 3) %>%
            RemoveInfluencers(., lm(y~., .), 4)
}

CleanTons <- function(df, TargetFUN = TargetPrice) {
  df %>%
    .SimplifyBuildingClass(.) %>%
      CleanColsStricter(., TargetFUN = TargetFUN) %>%
        CleanColsRatio(.) %>%
          CleanColsAssessment(.) %>%
            CleanCoordinates(.) %>%
              dplyr::filter(numbldgs == 1) %>%
                dplyr::select(-numbldgs)
}

CleanLog <- function(df, TargetFUN = TargetTaxPrevious) {
  df %>%
    CleanCols(., TargetFUN)
}


