
library(magrittr)
library(readr)
library(here)
library(dplyr)
library(tidyr)

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

LoadBrooklynSalesMap <- function(...) {
  files = list.files(here::here("data/sales-map"), "brooklyn_sales_map.*Part\\d\\.csv", full.names = T) %>%
    c(., list.files(here::here("data/sales-map"), "brooklyn_sales_map.*Part\\d\\d\\.csv", full.names = T))
  
  indexCol = "IndexColX"
  
  lapply(files, .ReadCsv, ...) %>%
    lapply(., .FixFirstColname, indexCol) %>%
      do.call(cbind, .) %>%
        .[, ! grepl(indexCol, colnames(.))]
}

LoadDoF <- function(...) {
  files = list.files(here::here("data/dof-rolling-sales"), "brooklyn", full.names = T)
  
  years = files %>%
    gsub(".*(\\d\\d)\\D.*", "\\1", .) %>%
      .[nchar(.) == 2] %>%
        sort()
  
  sortedFiles = sapply(years, grep, files, value = TRUE)
  headerFix = lapply(sortedFiles, .ReadXls, n_max = 10) %>%
    sapply(., function(x) { min(which(! is.na(x[, 2]))) })
  
  mapply(sortedFiles, headerFix, years, SIMPLIFY = FALSE, FUN = function(x, y, z) {
    .ReadXls(x, skip = y, ...) %>%
      magrittr::set_colnames(., gsub("\\n$", "", colnames(.))) %>%
        dplyr::mutate(FilenameYear = z)
  }) %>%
    do.call(rbind, .)
}

LoadPluto <- function(...) {
  .ReadCsv(here::here("data/pluto", "pluto_23v3.csv"), ...) %>%
    subset(., borough == "BK")
}

LoadMarketData <- function(dropMissing = TRUE, indexPos = seq_len(6),
                           indexName = c("CPIAUCSL", "GDP", "FEDFUNDS", "UNRATE", "CSUSHPISA")) {
  cpi = .ReadCsv(here::here("data/fred", "CPIAUCSL.csv"))
  fed = .ReadCsv(here::here("data/fred", "FEDFUNDS.csv"))
  gdp = .ReadCsv(here::here("data/fred", "GDP.csv"))
  unemp = .ReadCsv(here::here("data/fred", "UNRATE.csv"))
  shiller = .ReadCsv(here::here("data/fred", "CSUSHPISA.csv"))
  
  market = gdp %>%
    rbind(
      .,
      dplyr::mutate(., DATE = lubridate::floor_date(DATE - 15, "month"))[-1, ],
      dplyr::mutate(., DATE = lubridate::floor_date(DATE - 45, "month"))[-1, ]
    ) %>%
      dplyr::full_join(cpi, ., by = "DATE") %>%
        dplyr::full_join(., fed, by = "DATE") %>%
          dplyr::full_join(., unemp, by = "DATE") %>%
            dplyr::full_join(., shiller, by = "DATE") %>%
              .[, (seq_len(ncol(.)) %in% c(1, indexPos)) & (colnames(.) %in% c("DATE", indexName))]
  
  if (dropMissing[[1]]) {
    market = tidyr::drop_na(market)
  }
  
  market
}

.LoadMarketDataOld <- function(dropMissing = TRUE) {
  cpi = .ReadCsv(here::here("data", "data-cpi.csv"))
  fed = .ReadCsv(here::here("data", "data-fed.csv"))
  gdp = .ReadCsv(here::here("data/fred", "GDP.csv"))
  unemp = .ReadCsv(here::here("data", "data-unemp.csv"))
  
  market = gdp %>%
    rbind(
      .,
      dplyr::mutate(., DATE = lubridate::floor_date(DATE - 15, "month")),
      dplyr::mutate(., DATE = lubridate::floor_date(DATE - 45, "month"))
    ) %>%
      dplyr::full_join(cpi, ., by = c("Date" = "DATE")) %>%
        dplyr::full_join(., fed, by = c("Date" = "DATE")) %>%
          dplyr::full_join(., unemp, by = "Date")
  
  if (dropMissing[[1]]) {
    market = tidyr::drop_na(market)
  }
  
  market
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

PrepareDoF <- function(df) {
  df %>%
    subset(., `SALE PRICE` > 100) %>%
      .[, ! colnames(.) %in% c("EASE-MENT")]
}

AggregateMarket <- function(df, dateCol = "SALE DATE", dropMissing = TRUE) {
  market = LoadMarketData(TRUE)
  
  df[["DateFloored"]] = lubridate::floor_date(df[[dateCol]], "month")
  agg = dplyr::left_join(df, market, by = c("DateFloored" = "DATE"))
  
  if (dropMissing) {
    agg = agg[agg$DateFloored <= max(market$DATE) & agg$DateFloored >= min(market$DATE), ]
  }
  
  agg$DateFloored = NULL
  agg
}

AggregateBrooklyn <- function(df, authority) {
  dplyr::left_join(df, authority, by = c("BLOCK" = "block", "LOT" = "lot")) %>%
    .[! is.na(.$bbl), ] %>%
      .[, colSums(is.na(.)) / nrow(.) < 0.5] %>%
        tidyr::drop_na(.)
}

PipelineBrooklyn <- function(...) {
  rolling = PrepareDoF(LoadDoF(...))
  pluto = LoadPluto()
  AggregateBrooklyn(rolling, pluto)
}
