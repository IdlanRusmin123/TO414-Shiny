library(shiny)
library(ggplot2)

dataset <- diamonds

fluidPage(
  
  titlePanel("Transfer Value Estimate"),
  
  sidebarLayout(
    sidebarPanel(
      h2("tuto_title"),
      h5("tuto_text"),
    ),
    
    mainPanel(
      textInput("transfer", "TransferMarket Link", value = "https://www.transfermarkt.com/ollie-watkins/profil/spieler/324358"),
      textInput("fbref", "FBRef Link", value = "https://fbref.com/en/players/aed3a70f/Ollie-Watkins"),
      
      textOutput("prediction")
    )
  )
)