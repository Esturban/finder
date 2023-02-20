#' Check if the email address is valid
#'
#' @description Validate an email address with a regular expression 
#' to determine if the email can be contacted
#'
#' @param email The email address to check for validation
#' 
#' @return logical (TRUE or FALSE)
validate_email <-
  function(email)
    grepl(
      "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
      as.character(email),
      ignore.case = TRUE
    )
