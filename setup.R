# setup.R
#Library dependencies
require(purrr)
require(dplyr)
require(jsonlite)
require(here)
require(DT)
require(rvest)
require(apexcharter)


#' 
#' Extracting the domain name from a string 
#' @description The domain name is an element of a URL necessary for categorizing clients. This function will strip all of the unnecessary components of the URL so that the domain name remains
domain <-
  function(x)
    strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
