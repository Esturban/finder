#' Convert NULL values to NA for character type
#'
#' This function accepts an input value and returns NA if the value is NULL.
#'
#' @param item The input value to convert.
#'
#' @return The input value if not NULL, or NA if it is NULL.
#'
#' @examples
#' nullif(item = NULL)
#'
#' @export
nullif <- function(item) {
  ifelse(is.null(item), NA_character_, item)
}

#' Convert NULL values to NA for numeric type
#'
#' This function accepts an input value and returns NA if the value is NULL.
#'
#' @param item The input value to convert.
#'
#' @return The input value if not NULL, or NA if it is NULL.
#'
#' @examples
#' nullifN(item = NULL)
#'
#' @export
nullifN <- function(item) {
  ifelse(is.null(item), NA_real_, item)
}

#' Convert NULL values to NA for integer type
#'
#' This function accepts an input value and returns NA if the value is NULL.
#'
#' @param item The input value to convert.
#'
#' @return The input value if not NULL, or NA if it is NULL.
#'
#' @examples
#' nullifI(item = NULL)
#'
#' @export
nullifI <- function(item) {
  ifelse(is.null(item), NA_integer_, item)
}
