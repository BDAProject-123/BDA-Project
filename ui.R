library(shiny)

shinyUI(fluidPage(
  titlePanel("🛒 E-Commerce Recommender System - Top Rated Products"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select a Product Category Keyword:", choices = NULL)
    ),
    
    mainPanel(
      h4("Top Rated Products"),
      uiOutput("product_cards")
    )
  )
))
