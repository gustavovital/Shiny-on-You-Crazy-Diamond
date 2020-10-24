# Descrição: Shiny app com o objetivo de apresentar as curvas IS-LM. 
# Posteriormente pode ser implementada a curva BP.
# 
# Autor: Gustavo Vital
# Data: 23/20/2020

# Pacotes necessários ----

library(tidyverse)
library(shiny)
library(plotly)



ui <- fluidPage(
  
  titlePanel("Simulating IS-LM Models"),
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      fluidRow(
        tags$head(
          tags$style(type="text/css","label{ display: table-cell; text-align: center;vertical-align: middle; } .form-group { display: table-row;}") 
        ),
        column(11,style='background-color:#f2f2f2;min-width: 300px;',
               h4("Parameters"),
               tags$table(
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Marginal Propensity to Consume")),
                         tags$td(width = "20%", numericInput("c", NULL, value = 0.63))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Sensibility of the Investment to the Interest Rate")),
                         tags$td(width = "20%", numericInput("b", NULL, value = 1500))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Sensibility of the Money Demand to the Product")),
                         tags$td(width = "20%", numericInput("k", NULL, value = 0.6))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Sensibility of the Money Demand to the Interest Rate")),
                         tags$td(width = "20%", numericInput("h", NULL, value = 2700))),
                 br(),
               )
        )
      ),
      br(),
      fluidRow(
        tags$head(
          tags$style(type="text/css","label{ display: table-cell; text-align: center;vertical-align: middle; } .form-group { display: table-row;}") 
        ),
        column(11,style='background-color:#f2f2f2;min-width: 300px;',
               h4("Exogenous Variable"),
               tags$table(
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Autonomous Consumption")),
                         tags$td(width = "20%", numericInput("C_bar", NULL, value = 55))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Autonomous Investment")),
                         tags$td(width = "20%", numericInput("I_bar", NULL, value = 75))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Government Spending")),
                         tags$td(width = "20%", numericInput("G_bar", NULL, value = 200))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Tax on Income")),
                         tags$td(width = "20%", numericInput("T_bar", NULL, value = 110))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Money Supply")),
                         tags$td(width = "20%", numericInput("M_bar", NULL, value = 200))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Price Level (fixed the short-run)")),
                         tags$td(width = "20%", numericInput("P_bar", NULL, value = 1))),
                 br()
               )
        )
      )
    ),
    
    mainPanel(h1('IS-LM Simulation'),
      plotlyOutput("islmPlot")
    )
  )
  
  
  
)


server <- function(input, output) {
  
  output$islmPlot <- renderPlotly({
    
    # Matrix representation ----
    A <- rbind(c(1, -1, -1, 0),
               c(-input$c, 1, 0, 0),
               c(0, 0, 1, input$b),
               c(k, 0, 0, -input$h))
    d <- c(input$G_bar, input$C_bar-input$c*input$T_bar, input$I_bar, 
           input$M_bar/input$P_bar)
    
    # Solutions ----
    x <- solve(A, d)
    
    Y <- seq(0.95*x[1], 1.05*x[1], 1)
    C <- input$C_bar + input$c*(Y - input$T_bar)
    I <- Y - C - input$G_bar
    IS <- (input$I_bar-I)/input$b
    LM <- 1/input$h*(input$k*Y-input$M_bar/input$P_bar)
    
    data <- tibble(Y, C, I, IS, LM)
    
    ggplot(data, aes(x = Y)) + 
      geom_line(aes(y = IS), size = 2, alpha = .7, colour = 'dodgerblue3') +
      geom_line(aes(y = LM), size = 2, alpha = .7, colour = 'tomato3') +
      theme_minimal()
    
  })
  
}

shinyApp(ui, server)
