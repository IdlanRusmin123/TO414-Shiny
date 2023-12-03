library(shiny)

source("https://raw.githubusercontent.com/IdlanRusmin123/TO414-Shiny/master/main.R")

fluidPage(
  
  titlePanel("2022-2023 Season Big 5 Leagues Strikers Transfer Value Estimate"),
  
  mainPanel(
    h2("Description:"),
    p("This Shiny app is designed to estimate the value of strikers in the Big 5
      Leagues (EPL, Bundesliga, La Liga, Serie A and Ligue 1). To use this app,
      just put the full name (Case Sensitive and also Accent sensitive! Mbappé 
      need to be spelled with é, not e) below and click simulate. It has been 
      defaulted to Ollie Watkins, the striker of Aston Villa but you can just 
      change it."),
    uiOutput("tab"),
    
    h2("How to read output:"),
    p("The table outputted is the predictors used to calculate this striker value (except name). 
      The prediction is the value of the player that the model predicts.
      The given is the value of the player in the transfer market now (some players are overvalued and vice versa).
      The last sentence will say if the player is overvalued or undervalued"),
    
    textInput("name_given", "Name", value = "Ollie Watkins"),

    
    actionButton("simulate", "Simulate!"),
    textOutput("run"),
    h3("Predictors Used:"),
    tableOutput("table1"),
    tableOutput("table2"),
    textOutput("prediction"),
    textOutput("given"),
    textOutput("conclusion"),
  )

)