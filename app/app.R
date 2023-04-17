source("setup.R",F)
reactlog::reactlog_enable()

shinyApp(
  ui = ui(),
  server = function(input, output, session) {
    #First picture and quote
    ye_pic <-
      reactiveVal(value = random_img())
    ye_quote <-
      reactiveVal(value = random_quote())
    
    callModule(id = "ye",nav_server)
    callModule(id = "ye",panel_server)
    callModule(id = "ye",main_server,pic = ye_pic, bquote = ye_quote)
    #Determine if the application has been pulled for refreshing
    observeEvent(input$ptr,
                 {
                     print(ye_pic())
                   ye_pic(random_img())
                   ye_quote(random_quote())
                 },ignoreInit = F)
    # #Output of the card showing the yeezy quote and the picture
    # output[['yeezy']] <- renderUI({
    #     print(ye_pic())
    #     req(ye_pic())
    #   f7Card(image = ye_pic(),
    #     title = h2("Kanye West"),
    #     # shiny::img(src = paste0(), style = "display: block; margin-left: auto; margin-right: auto; max-width:100%; width:80%; height:auto; border-radius:3%;"),
    #     br(),
    #     shiny::img(shiny::blockquote(ye_quote()), style = "display: block; margin-left: auto; margin-right: auto; text-align:center;")
    #   )
    })
    # send the theme to javascript
    observe({
      session$sendCustomMessage(type = "ui-tweak",
                                message = list(os = input$theme, skin = input$color))
    })
    
  }
)
