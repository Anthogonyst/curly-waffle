

library(magrittr)
library(here)

#' Prefer local files, otherwise read from web.
#'
#' @param file The name of the file.
#' @param url Access a website for matching filename as a backup.
#' @param path Defaults to ~/data, otherwise any data folder allowed.
#' @param ReadFUN A closure to read the file into an object, default read.csv.
#' @author Anthogonyst
LoadDataElseWeb <- function(file, url = NULL, path = here::here("./data/"), ReadFUN = read.csv) {
  paste0(path, "/", file) %>%
    ifelse(file.exists(.), ., paste0(url, "/", file)) %>%
      ReadFUN(.)
}

#' Safe version to select columns whereas dplyr::select will crash on missing columns
#' 
#' @param df The dataframe.
#' @param columns The columns to remove if present
#' @param invert A boolean value to invert results (select none except columns)
#' @author Anthogonyst
RemoveCols <- function(df, columns, invert = FALSE) {
  if (invert) {
    df[, colnames(df) %in% columns]
  } else {
    df[, ! colnames(df) %in% columns]
  }
}

#' Parses all files in the path that match a regex to return the most recent one
#' @author Anthogonyst
.MostRecentOf <- function(path, regex = "*") {
  filesInfo = path %>%
    list.files(., full.names = TRUE, pattern = regex, ignore.case = TRUE) %>%
      file.info()
  
  mostRecentFile = rownames(filesInfo)[which.max(filesInfo$mtime)]
  return(mostRecentFile)
}

#' Sends the file to the server but without slowing you down
#' NOTE: If ran using R CMD, may terminate early without finishing if output object is not queried
#' 
#' @author Anthogonyst
SendToNetworkAsync <- function(from, to) {
  processCall = callr::r_bg(function(x, y) {
    exitcode = file.copy(x, y)
    out = c(
      paste("Attempted sending:", x),
      paste("To this location:", y),
      paste("Operation successful:", exitcode)
    )
    
    return(out)
  }, list(from, to))
  
  return(processCall)
}

WaitUntilAsyncDone <- function(..., verbose = FALSE) {
  processes = list(...)
  
  Status <- function(processes) {
    sapply(processes, function(x) {
      x$get_exit_status()
    })
  }
  
  notDone = any(sapply(Status(processes), is.null))
  
  if (notDone) {
    if (verbose) {
      print("Program almost done! Awaiting threads to finish execution.")
    }
    
    while(notDone) {
      Sys.sleep(3)
      notDone = any(sapply(Status(processes), is.null))
    }
    
    if (verbose) {
      print("Done for real now!")
    }
  }
  
  return(Status(processes))
}

#' Returns the date and time in YYYYMMDD_HHMMSS format
#'
#' Additionally adds a variable dateAndTime if non-existent to the environment.
#' Note this may cause undesirable effects if you write a bunch of files without
#' permanently allocating it somewhere, eg only temporarily making it in multiple functions
#' @author Anthogonyst
.DateAndTime <- function(update = FALSE) {
  if (exists("dateAndTime") && !update)
    return(dateAndTime)
  else {
    dateAndTime = Sys.time() %>% 
      substring(1, 19) %>% 
        gsub("-|:", "", .) %>%
          gsub(" ", "_", .)
    
    assign("dateAndTime", dateAndTime, envir = .GlobalEnv)
    return(dateAndTime)
  }
}

#' @author Anthogonyst
.PreviewColorPalette <- function(pal) {
  structure(pal, class = "palette", name = "Preview")
}

#' @author Anthogonyst
.SamplePalette <- function(pal1 = "Royal2", n1 = 4, t1 = "continuous", 
                           pal2 = "BottleRocket1", n2 = 3, t2 = "discrete", 
                           FUN = wesanderson::wes_palette) {
  structure(
    c(
      FUN(pal1, n1, t1), 
      FUN(pal2, n2, t2)
    ), 
    class = "palette",
    name = "Preview")
}

#' See here::here
#' 
#' here() uses a reasonable heuristics to find your project's files, based on 
#' the current working directory at the time when the package is loaded. Use it 
#' as a drop-in replacement for file.path(), it will always locate the files 
#' relative to your project root.
#' 
#' @param ...	[character] Path components below the project root, can be empty. 
#' Each argument should be a string containing one or more path components 
#' separated by a forward slash "/".
#' @author Anthogonyst
ThePath <- function(..., local = FALSE) {
  if (local[[1]]) {
    dir = "."
  } else {
    dir = here::here()
  }
  
  paste(dir, ..., sep = "/")
}

#' Evaluates test code that uses return, for, or otherwise has side effects that wish to be tested further within the current environment
#' 
#' @param code Test code containing expectations. Braces ({}) should always be used in order to get accurate location data for test failures.
#' @author Anthogonyst
.DoAs <- function(code) { 
  eval(substitute(code), testthat::test_env())
}

#' Plots an object as 4 combined plots, especially useful for the output of stats::lm
#' @author Anthogonyst
.PlotFour <- function(...) {
  graphics::par(mfrow=c(2,2))
  graphics::plot(...)
  graphics::par(mfrow=c(1,1))
  invisible(NULL)
}

#' Plots an object as 2 combined plots, especially useful for related graphs
#' @author Anthogonyst
.PlotTwo <- function(...) {
  graphics::par(mfrow=c(2,1))
  graphics::plot(...)
  graphics::par(mfrow=c(1,1))
  invisible(NULL)
}

#' Sets the rowname to a column specified, typically an index column
#' @author Anthogonyst
RowIndex <- function(df, indCol = "INDEX") {
  ind = df[[indCol]]
  rownames(df) = ind
  df
}

