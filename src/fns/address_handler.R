#' Naive address handler
#'
#' @description This is a naive address handler that will break out a common
#' string into different address components that are in line with 911 addresses
#' in Canada and USA. This naive address handler is only accustomed to handling
#' 3 scenarios based on the number of elements parsed in the first step
#'
#' @param address a full length string of the address, should measure to be a
#' single character
#' @param delim The delimiter that separates out the address into the components
#' found within address.
#' @return `tibble` of address components
#' @importFrom dplyr mutate
#' @importFrom tidyr spread

address_handler <- function(address, delim = ",") {
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  #Break up the components of the address and the total number of list items
  res <- unlist(strsplit(x = address, delim))
  #If there are 4 elements in the array...
  if (length(res) == 4) {
    #Name the address, city and prov_postal_code (interim)
    df_res <-
      setNames(res, c("address_1", "city", "prov_postal_code", "country")) %>%
      #Convert it into a data frame
      stack %>%
      #Spread the data frame to be columnar
      tidyr::spread("ind", "values") %>%
      #Clean up the elements separately
      dplyr::mutate(
        address_1 = trimws(address_1),
        city = trimws(city),
        country = trimws(country),
        state = trimws(substr(prov_postal_code, 1, 3)),
        zipcode = trimws(substr(prov_postal_code, 4, 11))
      )
    df_res
  } else if (length(res) == 5) {
    #If there are 5 elements in the array...
    #Firstly set all of their names
    df_res <- setNames(res,
                       c(
                         "address_2",
                         "address_1",
                         "city",
                         "prov_postal_code",
                         "country"
                       )) %>%
      #Stack the list into a data frame
      stack %>%
      #Spread the values so that it is a column-wide
      #dataframe instead of long format
      tidyr::spread("ind", "values") %>%
      #Begin cleaning up the single elements of the addresses
      dplyr::mutate(
        address_2 = trimws(address_2),
        address_1 = trimws(address_1),
        city = trimws(city),
        country = trimws(country),
        state = trimws(substr(prov_postal_code, 1, 3)),
        zipcode = trimws(substr(prov_postal_code, 4, 11))
      )
    df_res
  } else if (length(res) == 3) {
    #If the length of the list is only 3 values in the array...
    df_res <-
      setNames(res, c("city", "prov_postal_code", "country")) %>%
      stack %>%
      tidyr::spread("ind", "values") %>%
      dplyr::mutate(
        city = trimws(city),
        country = trimws(country),
        state = trimws(substr(prov_postal_code, 1, 3)),
        zipcode = trimws(substr(prov_postal_code, 4, 11))
      )
    df_res
  } else {
    #If the address cannot be parsed, it returns the elements into single
    #line item in the tibble
    tibble(address_1 = address)
  }
}
