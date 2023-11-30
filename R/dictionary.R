
.CountDiscrepancies <- function(newData, oldData, previousResult) {
  if (is.na(previousResult)) {
    return(0)
  }
  
  if (newData == oldData) {
    previousResult
  } else {
    previousResult + 1
  }
}

.Count <- function(x, y, z) {
  length(x)
}

.Worsened <- function(newData, oldData, previousResult) {
  if (is.na(previousResult)) {
    return(FALSE)
  }
  
  newData < oldData
}

### Record up to 28 boolean events in binary
.AllHistory <- function(newData, oldData, previousResult) {
  if (is.na(previousResult)) {
    return(0)
  }
  
  currentN = floor(previousResult / 1000000000)
  previousResult = previousResult + 1000000000
  
  if (TRUE && currentN <= 28) {
    previousResult = previousResult + 2^currentN
  }
  
  previousResult
}

.BadHistory <- function(newData, oldData, previousResult) {
  if (is.na(previousResult)) {
    return(FALSE)
  }
  
  (newData < oldData) | previousResult
}

.BadHistoryVectorized <- function(newData, oldData, previousResult) {
  any(newData < oldData, na.rm = TRUE)
}

.VeryBadHistory <- function(newData, oldData, previousResult) {
  any(5/3*newData < oldData, na.rm = TRUE)
}

### Largely un-optimized but works somewhat like a hash table with a special lambda
Dictionary <- function(sortedDf, key, value, ParseFUN = function(x, y, z) { x }) {
  if (inherits(sortedDf, "data.frame")) {
    key = sortedDf[[key]]
    value = sortedDf[[value]]
  }
  
  firstEvaluation = ParseFUN(value[[1]], NA, NA)
  
  dict = data.frame(t = key[[1]], u = value[[1]], v = firstEvaluation)[FALSE, ]
  dict[seq_len(length(key)), ] = NA
  
  uniqueN = 1
  for (i in seq_len(length(key))) {
    if (sum(dict$t == key[[i]], na.rm = TRUE) == 0) {
      dict[uniqueN, ] = list(key[[i]], value[[i]], ParseFUN(value[[i]], NA, NA))
      uniqueN = uniqueN + 1
    } else {
      ind = min(which(dict$t == key[[i]]))
      dict[ind, ] = list(dict$t[[ind]], value[[i]], ParseFUN(value[[i]], dict$u[[ind]], dict$v[[ind]]))
    }
  }
  
  tidyr::drop_na(dict)
}

DictionaryOptimized <- function(sortedDf, key, value, ParseFUN = function(x, y, z) { x }) {
  if (inherits(sortedDf, "data.frame")) {
    key = sortedDf[[key]]
    value = sortedDf[[value]]
  }
  
  firstEvaluation = ParseFUN(value[[1]], NA, NA)
  keys = unique(key)
  
  dict = data.frame(t = key[[1]], u = value[[1]], v = firstEvaluation)[FALSE, ]
  dict[seq_len(length(keys)), ] = NA
  dict$t = keys

  mapply(keys, seq_len(length(keys)), FUN = function(t, i) {
    du = value[key == t]
    
    if (length(du) <= 1) {
      dict[i, ] = list(t, du, ParseFUN(du, NA, NA))
    } else {
      dv = ParseFUN(t, NA, NA)
      for (n in seq_len(length(du) - 1)) {
        dv = ParseFUN(t, du[[n]], dv)
      }
      dict[i, ] = list(t, du[[length(du)]], dv)
    }
  })
  
  tidyr::drop_na(dict)
}


DictionaryOptimizedMore <- function(sortedDf, key, value, ParseFUN = function(x, y, z) { x }) {
  if (inherits(sortedDf, "data.frame")) {
    key = sortedDf[[key]]
    value = sortedDf[[value]]
  }
  
  firstEvaluation = ParseFUN(value[[1]], NA, NA)
  keys = unique(key)
  
  dict = data.frame(t = key[[1]], u = value[[1]], v = firstEvaluation)[FALSE, ]
  
  mapply(keys, seq_len(length(keys)), SIMPLIFY = F, FUN = function(t, i) {
    df = dict
    du = value[key == t]
    
    if (length(du) <= 1) {
      df[1, ] = list(t, du, ParseFUN(du, NA, NA))
    } else {
      dv = ParseFUN(t, NA, NA)
      for (n in seq_len(length(du) - 1) + 1) {
        dv = ParseFUN(du[[n]], du[[n - 1]], dv)
      }
      df[1, ] = list(t, du[[length(du)]], dv)
    }
    
    df
  }) %>%
    do.call(rbind, .) %>%
      tidyr::drop_na(.)
}

DictionaryOptimizedMoreee <- function(sortedDf, key, value, ParseFUN = function(x, y, z) { x }) {
  if (inherits(sortedDf, "data.frame")) {
    key = sortedDf[[key]]
    value = sortedDf[[value]]
  }
  
  firstEvaluation = ParseFUN(value[[1]], NA, NA)
  keys = unique(key)
  
  dict = data.frame(t = key[[1]], u = value[[1]], v = firstEvaluation)[FALSE, ]
  
  mapply(keys, seq_len(length(keys)), SIMPLIFY = F, FUN = function(t, i) {
    du = value[key == t]
    
    if (length(du) <= 1) {
      data.frame(t = t, u = du, v = ParseFUN(du, NA, NA))
    } else {
      dv = ParseFUN(t, NA, NA)
      for (n in seq_len(length(du) - 1) + 1) {
        dv = ParseFUN(du[[n]], du[[n - 1]], dv)
      }
      data.frame(t = t, u = du[[length(du)]], v = dv)
    }
  }) %>%
    do.call(rbind, .)
}

DictionaryVec <- function(sortedDf, key, value, ParseFUN = function(x, y, z) { x }) {
  if (inherits(sortedDf, "data.frame")) {
    key = sortedDf[[key]]
    value = sortedDf[[value]]
  }
  
  firstEvaluation = ParseFUN(value[[1]], NA, NA)
  keys = unique(key)

  dict = data.frame(t = key[[1]], u = value[[1]], v = firstEvaluation)[FALSE, ]
  
  ldf = lapply(keys, function(t) {
    du = value[key == t]
    data.frame(t = t[[1]], u = du[[length(du)]], v = ParseFUN(du, data.table::shift(du, 1), NA))
  })
  
  do.call(rbind, ldf)
}

DictionaryVectorized <- function(sortedDf, key, value, ParseFUN = function(x, y, z) { x }) {
  if (inherits(sortedDf, "data.frame")) {
    key = sortedDf[[key]]
    value = sortedDf[[value]]
  }
  
  firstEvaluation = ParseFUN(value[[1]], NA, NA)
  keys = unique(key)
  
  dict = list(
    t = keys,
    u = rev(keys) %>% duplicated(.) %>% which(.) %>% { value[.] },
    v = sapply(keys, function(t) {
      du = value[key == t]
      ParseFUN(du, data.table::shift(du, 1), NA)
    })
  )
  
  dict
}
