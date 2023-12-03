library(shiny)

source("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/main.R")

fluidPage(
  
  titlePanel("Transfer Value Estimate"),
  
  mainPanel(
    h2("tuto_title"),
    h5("tuto_text"),
    textInput("name_given", "Name", value = "Ollie Watkins"),
    
    actionButton("simulate", "Simulate!"),
    textOutput("run"),
    tableOutput("table1"),
    tableOutput("table2"),
    textOutput("prediction")
  )

)