#' Import the manual review of matches
#'
#' @export
#'
#' @author Finlay Campbell (\email{finlaycampbell93@@gmail.com})
#'
#' @param file The file path to the completed review document, with the
#'   "reviewer_" columns filled out
#'
#' @param dictionary Map the values of the "reviewer_" columns to the accepted
#'   outcomes "accept", "reject" or "discuss". This can be a named list
#'   indicating which results map to which of the three outcome, or a function
#'   accepting a character vector and outputting a character vector of outcomes
#'   of the same length.
#'
#' @importFrom openxlsx read.xlsx

import_review <- function(file,
                          dictionary = list(accept = c("accept", "yes"),
                                            reject = c("reject", "no"),
                                            discuss = c("discuss", "maybe"))) {

  ## import
  res <- read.xlsx(file)

  ## define mapping function
  if(is.function(dictionary)) {
    get_outcome <- function(x) dictionary(as.character(x))
  } else {
    if(length(dictionary) != 3 |
       !all(sort(names(dictionary)) == c("accept", "discuss", "reject"))) {
      stop("dictionary must be of length three and specify 'accept', 'reject' and 'discuss'")
    }
    get_outcome <- function(x) {
      x <- as.character(x)
      for(i in names(dictionary)) x[x %in% dictionary[[i]]] <- i
      x[!x %in% names(dictionary)] <- NA
      return(x)
    }
  }

  ## map results to outcomes
  reviewer_ind <- grepl("reviewer_", names(res))
  res[reviewer_ind] <- lapply(res[reviewer_ind], get_outcome)

  ## get consensus decision from the reviewers
  get_consensus <- function(x) {
    if(all(is.na(x))) {
      return('unreviewed')
    } else if(all(na.omit(x) == 'accept')) {
      return('accepted')
    } else if(all(na.omit(x) == 'reject')) {
      return('rejected')
    } else {
      return('discuss')
    }
  }

  ## return review split by decision
  return(split(res, apply(res[reviewer_ind], 1, get_consensus)))

}
