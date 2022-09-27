require(shinyMobile)
require(shiny)
require(shinyWidgets)
require(apexcharter)
require(rvest)
# setwd("app")
# source modules
path <- "src/"

#Source the scripts in src folder
#Includes the PWA dependencies
sapply(list.files(
  path,
  include.dirs = F,
  pattern = '.R',
  ignore.case = T
),
function(f) {
  source(paste0(path, f), F)
})

#Remaining libraries to load
library(tidyverse)
library(jsonlite)

#Load the UI modules
path<-"ui/"
sapply(list.files(
  path,
  include.dirs = F,
  pattern = '.R',
  ignore.case = T
),
function(f) {
  source(paste0(path, f), F)
})

