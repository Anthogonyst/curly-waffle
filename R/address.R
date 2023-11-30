
library(dplyr)
library(lubridate)

CleanAddress <- function(x) {
  toupper(x)
}

SearchAddress <- function(x, CleanFUN = CleanAddress, strictModel = FALSE) {
  pluto = LoadPluto()
  
  df = data.frame(ADDRESS = CleanFUN(x)) %>%
    dplyr::inner_join(., pluto, by = "ADDRESS")
  
  if (nrow(df) <= 0) {
    stop("Did not find any addresses")
  }
  
  if (strictModel) {
    df = CleanCols(df)
  } else {
    df = CleanColsStricter(df)
  }
  
  df$saledate = lubridate::today()
  df
}
