## check by
.assert_by <- function(x, y, by) {

  if(is.vector(by)) {
    if(!inherits(by, "character")) {
      stop("by must be a character vector")
    }
    col_match <- matrix(c(match(by, names(x)),
                          match(by, names(y))),
                        ncol = 2)
    if(any(is.na(col_match[,1]))) {
      stop(paste(by[is.na(col_match[,1])], collapse = ", "), " not found in x")
    }
    if(any(is.na(col_match[,2]))) {
      stop(paste(by[is.na(col_match[,2])], collapse = ", "), " not found in y")
    }
  } else if(is.matrix(by)) {
    if(ncol(by) != 2) {
      stop("by must be a matrix with two columns")
    }
    if(is.character(by)) {
      col_match <- matrix(c(match(by[,1], names(x)),
                            match(by[,2], names(y))),
                          ncol = 2)
      if(any(is.na(col_match[,1]))) {
        stop(paste(by[is.na(col_match[,1]), 1], collapse = ", "), " not found in x")
      }
      if(any(is.na(col_match[,2]))) {
        stop(paste(by[is.na(col_match[,2]), 2], collapse = ", "), " not found in y")
      }
    } else if(is.numeric(by)) {
      if(any(by[,1] > ncol(x), by[,2] > ncol(y))) {
        stop("a value in by is greater than the number of columns in x or y")
      }
      col_match <- by
    } else {
      stop("by must be a character or numeric matrix")
    }
  } else {
    stop("by must be a matrix or character vector")
  }
  return(col_match)
}

## check than columns classes match
.assert_col_class <- function(x, y, col_match) {

  class_1 <- vapply(x[,col_match[,1], drop = FALSE], class, "")
  class_2 <- vapply(y[,col_match[,2], drop = FALSE], class, "")

  class_1[class_1 == 'integer'] <- 'numeric'
  class_2[class_2 == 'integer'] <- 'numeric'

  if(!all(class_1 == class_2)) {
    stop("Matched columns must be of the same class")
  }

  rownames(class_1) <- NULL

  return(class_1)

}

## check score_fun
.assert_score_fun <- function(score_fun, classes) {
  if(!is.null(score_fun)) {
    if(!all(vapply(score_fun, inherits, TRUE, "function"))) {
      stop("all elements of score_fun must be a function")
    }
    if(!all(names(score_fun) %in% names(classes))) {
      stop("score_fun must map to variables in the first column of by")
    }
    arg_len <- vapply(score_fun, function(fun) length(formals(fun)), 1L)
    if(!all(arg_len %in% c(2, 3))) {
      stop("functions in score_fun must accept two or three arguments")
    }
  }
  return(score_fun)
}

## generate matching functions with dist value embedded
.create_score_fun <- function(class) {
  if(class %in% c("character", "factor")) {
    return(function(a, b) stringdist::stringdist(a, b))
  } else if(class == "numeric") {
    return(function(a, b) -abs(a - b))
  } else if(class %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")) {
    return(function(a, b) - abs(as.numeric(a - b)))
  } else {
    stop("column class must be numeric, Date or character")
  }
}

## check whether
.assert_raw <- function(f) {
  formal <- formals(f)
  if(length(formal) == 2) {
    return(FALSE)
  } else {
    if(!is.logical(formal[[3]])) {
      stop("the third argument of a matching function must be a logical")
    } else {
      return(formal[[3]])
    }
  }
}

## rescale a vector of numerics to min and max
.rescale <- function(x, min_val = 0, max_val = 1) {
  if(length(unique(x)) == 1L) return(x)
  out <- (x - min(x, na.rm = TRUE))/(diff(range(x, na.rm = TRUE)))*(max_val - min_val)
  out <- out + min_val
  return(out)
}
