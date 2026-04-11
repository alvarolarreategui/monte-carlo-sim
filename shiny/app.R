# NVP Shiny web app
# ISyE6644 Simulation Project
# Author: Alvaro Larreátegui

# Packages
if(!require("shiny")) install.packages("shiny")
if(!require("bslib")) install.packages("bslib")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("DescTools")) install.packages("DescTools")
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(DescTools)
source("helpers.R")

# Global variable
lambda <- 1/36.4

# Define UI for app ----
ui <- page_sidebar(
  title = "ISyE6644 Project - NVP Application: 
  Optimizing the Production of a French Bakery",
  sidebar = sidebar(
    card(
      "______ GOAL  ______  Optimize Daily Prodution to Maximize the Profit,
       using Monte Carlo Simulation _________________"
    ),
    "Business Parameters", 
    card("Unit Costs (euros)",
         helpText("Baguette .......... 0.90"),
         helpText("Croissant ......... 0.80"),
         helpText("Pain au Chocolat..  0.85"),
         helpText("Coupé ............. 0.10"),
         helpText("Banette ........... 0.85"),
         ),
    card("Unit Margins (euros)",
         helpText("Baguette .......... 0.40"),
         helpText("Croissant ......... 0.40"),
         helpText("Pain au Chocolat... 0.45"),
         helpText("Coupé ............. 0.05"),
         helpText("Banette ........... 0.30"),
    ),
    card("No Salvage Value",
         helpText("We assume that unsold product is 100% loss"),
    ),
  ),
  # App title ----
  titlePanel("Monte Carlo Simulation"),
  # Sidebar layout ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      card(
        "MC Simulation is used here to generate the daily demand of bakery products. 
        It is modeled as a COMPOUND DEMAND, ie the sum of two RVs: the DAILY CUSTOMER ARRIVALS and  
        the DEMAND FOR SPECIFIC ITEMS per customer. With this demand, daily profits are computed
        considering NVP: unsold items at the end of the day are lost, and unmet demand is penalized
        as a fraction of the margin. For each replication an Optimal Production Policy that maximizes 
        the Total Profit for the business cycle is found numerically. ",
        helpText("Note: Sundays are excluded."),
      ),
      card(
        # Input: Specify the seed
        numericInput("seed", "Simulation seed", 6644),
           ),
      card(
        numericInput("days", "Replication length (days):", 365),
        numericInput("reps", "Number of Replications:", 100),
        helpText("Note: allow to run for a couple of minutes for large values (>500).")
        ),
      card(
           numericInput("penalty", "Shortage Penalty as % of unit margin:", 10),
           ),
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      actionButton("update", "Run Simulation!"),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: CI ----
      h4("Mean and 95% CI for the Optimal Daily Production Policy (units):"),
      verbatimTextOutput("Qopt"),
      
      # Output: CI ----
      h4("Mean and 95% CI for the resulting Average Daily Profit (euros):"),
      verbatimTextOutput("Popt"),
      
      # Output: Histogram ----
      h4("Histogram of Average Daily Profit (Euros)"),
      plotOutput(outputId = "distPlot"),
      
      # Output: Empirical distribution ----
      h4("Empirical Distribution of Item Demand"),
      tableOutput("empirical"),
      
      # Output: Histogram of arrivals ----
      h4("Histogram of daily Arrivals"),
      h6("(example from one replication)"),
      plotOutput(outputId = "arrivPlot"),
      
      # Output: optimization plot ----
      h4("Optimization of profits "),
      h6("(example from one replication)"),
      plotOutput(outputId = "profitPlot"),
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # Return the results of the sim
  simResult <- eventReactive(input$update, {
    run_sims(lambda,
             input$reps, 
             input$days, 
             input$penalty, 
             input$seed)
  }, ignoreNULL = FALSE)
  
  genArrivals <- eventReactive(input$update, {
    arrivals(lambda, 
             input$days)
  }, ignoreNULL = FALSE)
  
  # Generate a CI ----
  output$Qopt <- renderPrint({
    data <- simResult()
    ceiling(apply(data[, 1:5], 2, MeanCI))
  })
  
  # Generate a CI ----
  output$Popt <- renderPrint({
    data <- simResult()
    MeanCI(data$AVG_PROFIT)
  })
  
  # Generate Profit histogram ----
  output$distPlot <- renderPlot({
    dataset <- simResult()
    hist(dataset$AVG_PROFIT, col = "#007bc2", border = "white",
         xlab = "Euros",
         main = "")
  })
  
  # Generate a view of empirical dist ----
  output$empirical <- renderTable({
    mat <- get_item_prob()
    matrix <- cbind(rownames(mat), round(mat, 5))
    colnames(matrix)[1] = " "
    matrix
  })
  
  # Generate histogram for arrivals ----
  output$arrivPlot <- renderPlot({
    dataset <- genArrivals()
    hist(dataset, col = "#007bc2", border = "white",
         main = "")
  })
    
  # Generate the optimization plot ----
  output$profitPlot <- renderPlot({
    arr <- genArrivals()
    plot_optimization(arr, input$penalty, 100)
  })
}

# Create Shiny app ----
shinyApp(ui, server)