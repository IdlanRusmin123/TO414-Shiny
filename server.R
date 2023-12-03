library(shiny)

function(input, output) {
  
  success <- eventReactive(input$simulate, {
    fun1(input$name_given)
  })
  
  x1 <- eventReactive(input$simulate, {
    if(success() == "Success!") {
      table_out1
    }
    
  })
  
  x2 <- eventReactive(input$simulate, {
    if(success() == "Success!") {
      table_out2
    }
    
  })
  
  x3 <- eventReactive(input$simulate, {
    if(success() == "Success!") {
      paste("test", pred)
    }
    
  })
  
  output$run <- renderText({
    success()
  })
  
  output$table1 <- renderTable(x1())
  output$table2 <- renderTable(x2())

  output$prediction <- renderText({
    x3()
  })
  
}