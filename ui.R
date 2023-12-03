library(shiny)

source("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/main.R")

fluidPage(
  
  titlePanel("Transfer Value Estimate"),
  
  mainPanel(
    h2("tuto_title"),
    h5("tuto_text"),
    textInput("fbref", "FBRef Link", value = "https://fbref.com/en/players/aed3a70f/Ollie-Watkins"),
    
    actionButton("simulate", "Simulate!"),
    tableOutput("table1"),
    tableOutput("table2"),
    textOutput("prediction")
  )

)