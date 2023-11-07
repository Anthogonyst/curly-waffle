
library(magrittr)
library(readr)
library(here)

.ReadCsv <- function(x, ...) {
  readr::read_csv(x, show_col_types = FALSE, name_repair = "minimal", progress = FALSE, ...)
}

.ReadXls <- function(x, ...) {
  readxl::read_excel(x, progress = FALSE, .name_repair = "minimal", ...)
}

.FixFirstColname <- function(df, name) {
  colnames(df)[1] = name
  df
}

LoadBrooklynSalesMap <- function() {
  files = list.files(here::here("data/sales-map"), "brooklyn_sales_map.*Part\\d\\.csv", full.names = T) %>%
    c(., list.files(here::here("data/sales-map"), "brooklyn_sales_map.*Part\\d\\d\\.csv", full.names = T))
  
  indexCol = "IndexColX"
  
  lapply(files, .ReadCsv) %>%
    lapply(., .FixFirstColname, indexCol) %>%
      do.call(cbind, .) %>%
        .[, ! grepl(indexCol, colnames(.))]
}

LoadDoF <- function() {
  files = list.files(here::here("data/dof-rolling-sales/"), "brooklyn", full.names = T)
  
  years = files %>%
    gsub(".*(\\d\\d)\\D.*", "\\1", .) %>%
      .[nchar(.) == 2] %>%
        sort()
  
  sortedFiles = sapply(years, grep, files, value = TRUE)
  headerFix = lapply(sortedFiles, .ReadXls, n_max = 10) %>%
    sapply(., function(x) { min(which(! is.na(x[, 2]))) })
  
  mapply(sortedFiles, headerFix, years, SIMPLIFY = FALSE, FUN = function(x, y, z) {
    .ReadXls(x, skip = y) %>%
      magrittr::set_colnames(., gsub("\\n$", "", colnames(.))) %>%
        dplyr::mutate(FilenameYear = z)
  }) %>%
    do.call(rbind, .)
}

PrepareBrooklyn <- function(df) {
  df %>%
    dplyr::mutate(
      sale_price = as.numeric(sale_price),
      sale_date = as.Date(sale_date, "%Y-%m-%d"),
      APPDate = as.Date(APPDate, "%m/%d/%Y")
    ) %>%
      dplyr::mutate_at(., colnames(.)[sapply(., is.character)], as.factor) %>%
        dplyr::rename(y = sale_price) %>%
          dplyr::select(y, dplyr::everything())
}
