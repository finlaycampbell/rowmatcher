#' Fuzzy matching of cases between linelists
#'
#' This function matches cases between linelists on specified columns using
#' user-specified matching thresholds.
#'
#' @export
#'
#' @author Finlay Campbell (\email{finlaycampbell93@@gmail.com})
#'
#' @param x A dataframe containing the columns specified in the first column of
#'   the \code{by} argument.
#'
#' @param y A dataframe containing the columns specified in the second column of
#'   the \code{by} argument.
#'
#' @param by Linelist columns to match cases on. This can be a character vector
#'   indicating column names found in both linelists, a 2-column integer matrix
#'   indicating the pairs of columns to be matched in linelist 1 and linelist 2,
#'   or a 2-column character matrix indicating the names of the columns to be
#'   matched in linelist 1 and linelist 2.
#'
#' @param score_fun An optional list of functions for customised evaluations of
#'   matches. Each function must accept two vectors as arguments and return a
#'   numeric vector of the same length indicating the quality of the match.
#'
#' @param rescale A logical indicating whether scores for each variable should
#'   be rescaled between 0 and 1.
#'
#' @param na_score A numeric indicating the score to be assigned to NA
#'   scores. NA handling can also be specified in a variable-specific manner by
#'   providing custom scoring functions to \code{score_fun}.
#'
#' @param output If "index", returns a dataframe of matched indices between the
#'   linelists. If "merged", returns a merged linelist. If "review", returns a
#'   dataframe for manual reviewing of matches.
#'
#' @return A dataframe of matching indices if output = "index", a merged
#'   linelist if output = "merged".
#'
#' @importFrom pbapply pbmapply
#'
#' @examples
#' data(sample_linelists)
#'
#' ## examine linelists
#' head(sample_linelists$linelist_a)
#' head(sample_linelist$linelist_b)
#'
#' ## specify matching columns
#' by <- matrix(c("numeric_a", "numeric_b",
#'                "character_a", "character_b",
#'                "date_a", "date_b"),
#'              ncol = 2, byrow = TRUE)
#'
#' ## find matching case indices
#' matches <- match_rows(linelist_a, linelist_b, by)
#' head(matches)
#'
match_rows <- function(x, y, by,
                       score_fun = NULL,
                       rescale = TRUE,
                       na_score = 0,
                       filter = c("none", "best", "min_score"),
                       output = c("index", "merged", "review"),
                       min_score = NULL) {

  ## match args
  filter <- match.arg(filter)
  output <- match.arg(output)

  ## check by
  by <- .assert_by(x, y, by)
  by_names <- matrix(c(names(x)[by[,1]], names(y)[by[,2]]), ncol = 2)

  ## check na_score
  if(!is.numeric(na_score) | length(na_score) != 1) {
    stop("na_score must be a numeric of length 1")
  }

  ## determine column classes
  classes <- .assert_col_class(x, y, by)

  ## check matching functions
  score_fun <- .assert_score_fun(score_fun, classes)

  ## generate matching functions
  f_list <- lapply(classes, .create_score_fun)
  f_list[names(score_fun)] <- score_fun

  ## check if a vectorised or non-vectorised function is provided
  raw <- lapply(f_list, .assert_raw)

  ## apply each element of f_list to the specified variables
  scores <- pbmapply(
    function(a, b, f, raw) if(raw) f(a, b) else outer(a, b, f),
    as.list(x[by[,1]]),
    as.list(y[by[,2]]),
    f_list, raw
  )

  ## insert NA scores
  scores[is.na(scores)] <- na_score

  ## rename score columns
  colnames(scores) <- paste0("match_score_", colnames(scores))

  ## rescale if required
  if(rescale) scores <- apply(scores, 2, .rescale)

  ## generate output
  out <- data.frame(
    expand.grid(index_x = seq_len(nrow(x)), index_y = seq_len(nrow(y))),
    match_score = rowSums(scores),
    scores
  )

  ## filter rows if required
  if(filter == "best") {
    out <- lapply(
      split(out, out$index_x),
      function(x) x[order(x$match_score, decreasing = TRUE)[1],]
    )
    out <- do.call(rbind, out)
  } else if(filter == "min_score") {
    if(is.null(min_score)) stop("min_score must be provided")
    out <- out[out$match_score >= min_score,]
  }

  ## merge in data and remove scores if required
  if(output %in% c("merged", "review")) {
    x$index_x <- seq_len(nrow(x))
    y$index_y <- seq_len(nrow(y))
    out <- merge(out, x, by = "index_x")
    out <- merge(out, y, by = "index_y")
    out <- out[with(out, order(index_x, index_y)),]
    rownames(out) <- NULL
  }

  if(output == "merged") {
    ## remove uncessary columns
    out[, c("index_x", "index_y", names(out)[grepl("match_score_", names(out))])] <- NULL
  } else if (output == "review") {
    ## re-order columns for review
    out <- out[c("match_score", "index_x", "index_y", as.vector(t(by_names)))]
  }

  return(out)

}
