path<-"ui/components/"
sapply(list.files(
  path,
  include.dirs = F,
  pattern = '.R',
  ignore.case = T,
  recursive = T
),
function(f) {
  source(paste0(path, f), .GlobalEnv)
})



ui <- function(title = "I love Kanye",opts = list(pullToRefresh = TRUE)) {
  f7Page(
    title = title,
    options = opts,
    allowPWA = T,
    f7TabLayout(
      panels = panel_ui(id = "ye"),
      navbar =nav_ui(id = "ye"),
      f7Tabs(id = 'tabs',
        animated = TRUE,
        main_ui("ye")
      )
    )
  )
}
