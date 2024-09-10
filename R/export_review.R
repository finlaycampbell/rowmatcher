#' Export the matching results for manual review
#'
#' @export
#'
#' @author Finlay Campbell (\email{finlaycampbell93@@gmail.com})
#'
#' @param matches The matching results as produced by \code{match_rows}, with
#'   the \code{output} argument set to "review"
#'
#' @param file The file path for the review document
#'
#' @param top_n The number of matches to be included per row in the manual review
#'
#' @param n_reviewers The number of reviewer columns to include
#'
#' @param date_format The date format to be written to excel
#'
#' @importFrom openxlsx write.xlsx loadWorkbook addStyle createStyle saveWorkbook

export_review <- function(matches,
                          file,
                          top_n = 5,
                          n_reviewers = 3,
                          date_format = "yyyy-mm-dd") {

  ## define empty row for output file
  empty_row <- matches[0,]
  empty_row[1,] <- NA

  ## round match score
  matches$match_score <- round(matches$match_score, 2)

  ## split by x index
  matches <- split(matches, matches$index_x)

  ## order by best score
  matches <- matches[
    order(
      vapply(matches, function(x) max(x$match_score, na.rm = TRUE), 1.0),
      decreasing = TRUE)
  ]

  ## extract top_n results per case and append empty row
  matches <- lapply(
    matches,
    function(x)
      rbind(
        empty_row,
        x[order(x$match_score, decreasing = TRUE)[seq_len(min(nrow(x), top_n))],]
      )
  )
  matches <- do.call(rbind, matches)
  rownames(matches) <- NULL

  ## insert reviewer columns and reshuffle
  reviewers <- paste0("reviewer_", seq_len(n_reviewers))
  matches[reviewers] <- NA
  ind <- c("index_x", "index_y", "match_score", reviewers)
  matches <- matches[c(ind, names(matches)[!names(matches) %in% ind])]

  ## write xlsx file
  old_format <- getOption("openxlsx.dateFormat")
  options(openxlsx.dateFormat = date_format)
  write.xlsx(matches, file = file, rowNames = FALSE, showNA = FALSE)
  options(openxlsx.dateFormat = old_format)

  ## load workbook for cell coloring
  wb <- loadWorkbook(file)

  ## add grey background
  addStyle(
    wb = wb,
    sheet = "Sheet 1",
    style = createStyle(bgFill = "lightgrey", fgFill = "lightgrey"),
    rows = which(apply(matches, 1, function(y) all(is.na(y)))) + 1,
    cols = seq_len(ncol(matches)),
    gridExpand = TRUE
  )

  ## export
  saveWorkbook(wb, file, overwrite = TRUE)

  return(NULL)

}
