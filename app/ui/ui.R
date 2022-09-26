path <- "ui/components/"
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



ui <-
  function(title = "I love Kanye",
           opts = list(pullToRefresh = TRUE)) {
    f7Page(
      title = title,
      options = opts,
      allowPWA = T,
      f7TabLayout(
        panels = tagList(
          f7Panel(
            title = "Left Panel",
            side = "left",
            theme = "light",
            "Blabla",
            effect = "cover"
          ),
          f7Panel(
            title = "Right Panel",
            side = "right",
            theme = "dark",
            "Blabla",
            effect = "cover"
          )
        ),
        navbar = f7Navbar(
          title = "Tabs",
          hairline = TRUE,
          shadow = TRUE,
          leftPanel = TRUE,
          rightPanel = TRUE
        ),
        f7Tabs(
          animated = TRUE,
          #swipeable = TRUE,
          f7Tab(
            tabName = "Tab 1",
            icon = f7Icon("folder"),
            active = TRUE,
            
            f7Flex(
              prettyRadioButtons(
                inputId = "theme",
                label = "Select a theme:",
                thick = TRUE,
                inline = TRUE,
                selected = "md",
                choices = c("ios", "md"),
                animation = "pulse",
                status = "info"
              ),
              
              prettyRadioButtons(
                inputId = "color",
                label = "Select a color:",
                thick = TRUE,
                inline = TRUE,
                selected = "dark",
                choices = c("light", "dark"),
                animation = "pulse",
                status = "info"
              )
            ),
            
            # tags$head(
            #   tags$script(
            #     'Shiny.addCustomMessageHandler("ui-tweak", function(message) {
            #     var os = message.os;
            #     var skin = message.skin;
            #     if (os === "md") {
            #       $("html").addClass("md");
            #       $("html").removeClass("ios");
            #       $(".tab-link-highlight").show();
            #     } else if (os === "ios") {
            #       $("html").addClass("ios");
            #       $("html").removeClass("md");
            #       $(".tab-link-highlight").hide();
            #     }
            # 
            #     if (skin === "dark") {
            #      $("html").addClass("theme-dark");
            #     } else {
            #       $("html").removeClass("theme-dark");
            #     }
            # 
            #    });
            #   '
            #   )
            # ),
            
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              f7Card(title = "Card header",
                     apexchartOutput("pie"))
            )
          ),
          f7Tab(
            tabName = "Tab 2",
            icon = f7Icon("keyboard"),
            active = FALSE,
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              f7Card(title = "Card header",
                     apexchartOutput("scatter"))
            )
          ),
          f7Tab(
            tabName = "Tab 3",
            icon = f7Icon("layers_alt"),
            active = FALSE,
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              f7Card(
                title = "Card header",
                f7SmartSelect(
                  "variable",
                  "Variables to show:",
                  c(
                    "Cylinders" = "cyl",
                    "Transmission" = "am",
                    "Gears" = "gear"
                  ),
                  openIn = "sheet",
                  multiple = TRUE
                ),
                tableOutput("data")
              )
            )
          )
        )
      )
    )
  }