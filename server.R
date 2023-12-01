library(shiny)
library(ggplot2)

lin_m <- readRDS("Linear_Reg.rds")
tree_m <- readRDS("Reg_Tree.rds")
rf_m <- readRDS("Rand_For.rds")
ann_m <- readRDS("ANN.rds")
svm_m <- readRDS("SVM.rds")
knn_m <- readRDS("KNN.rds")
stacked_m <- readRDS("Stacked.rds")

function(input, output) {
  
  output$prediction <- renderText({
    transfer <<- input$transfer
    fbref <<- input$fbref
    source("main.R")
    prediction
  })
  
}