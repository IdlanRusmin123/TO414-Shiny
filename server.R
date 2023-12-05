library(shiny)

function(input, output) {
  url <- a("https://fbref.com/en/", href="https://fbref.com/en/")
  output$tab <- renderUI({
    tagList("You can refer to fbref (", url, ") for searching and get their full name with correct case and accent")
  })
  
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
      paste("Prediction:", pred, "euros")
    }
    
  })
  
  x4 <- eventReactive(input$simulate, {
    if(success() == "Success!") {
      paste("Given:", given, "euros")
    }
    
  })
  
  x5 <- eventReactive(input$simulate, {
    if(success() == "Success!") {
      if(pred < given) {
        paste("The player is OVERvalued by", given - pred, "euros")
      } else {
        paste("The player is UNDERvalued by", pred - given, "euros")
      }

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
  
  output$given <- renderText({
    x4()
  })
  
  output$conclusion <- renderText({
    x5()
  })
}