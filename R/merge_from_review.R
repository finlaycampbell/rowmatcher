#' Merge two databases on the basis of manual reviews.
#'
#' @export
#'
#' @author Finlay Campbell (\email{finlaycampbell93@@gmail.com})
#'
#' @param x The first database to be merged
#'
#' @param y The second database to be merged
#'
#' @param review The list returned by \code{import_review} or a string
#'   indicating the file path of the review document
#'
#' @param ... Additional arguments passed to \code{import_review}

merge_from_review <- function(x, y, review, ...) {

  if(is.character(review)) review <- import_review(review, ...)

  if(!is.list(review)) stop("review must be a filepath or the list return by import_review")

  x$index_x <- seq_len(nrow(x))
  y$index_x <- NA
  y$index_x[review$accepted$index_y] <- review$accepted$index_x

  out <- merge(x, y, "index_x")
  out$index_x <- NULL

  return(out)

}
