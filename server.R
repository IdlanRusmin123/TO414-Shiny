library(shiny)

function(input, output) {
  
  x1 <- eventReactive(input$simulate, {
    fun1(input$fbref)
    table_out1
  })
  
  x2 <- eventReactive(input$simulate, {
    table_out2
  })
  
  x3 <- eventReactive(input$simulate, {
    paste("test", pred)
  })
  
  output$table1 <- renderTable(x1())
  output$table2 <- renderTable(x2())

  output$prediction <- renderText({
    x3()
  })
  
}