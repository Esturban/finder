source("setup.R",F)

#Load and run all of the UI files
sapply(list.files('src',full.names = T),source,local=F)
#Add the corresponding modules needed to run the files
sapply(list.files('modules',full.names = T),source,local=F)
#Load and run all of the UI files
sapply(list.files('ui',pattern = "*.R",full.names=T),source,local=F)

shinyApp(ui=ui(),server=function(input,output,session){
  # river plot
  dates <- reactive(seq.Date(Sys.Date() - 30, Sys.Date(), by = input$by))
  
  output$pie <- renderApexchart({
    apex(
      data = poll,
      type = "pie",
      mapping = aes(x = answer, y = n)
    )
  })
  
  output$scatter <- renderApexchart({
    apex(
      data = mtcars,
      type = "scatter",
      mapping = aes(
        x = wt,
        y = mpg,
        fill = cyl
      )
    )
  })
  
  
  # datatable
  output$data <- renderTable({
    mtcars[, c("mpg", input$variable), drop = FALSE]
  }, rownames = TRUE)
  
  
  # send the theme to javascript
  # observe({
  #   session$sendCustomMessage(
  #     type = "ui-tweak",
  #     message = list(os = input$theme, skin = input$color)
  #   )
  # })
})


