# Descrição: Shiny app com o objetivo de apresentar as curvas IS-LM. 
# Posteriormente pode ser implementada a curva BP.
# 
# Autor: Gustavo Vital
# Data: 23/20/2020

# Pacotes necessários ----

library(shiny)

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
                         tags$td(width = "20%", numericInput("num", NULL, value = 0.63))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Sensibility of the Investment to the Interest Rate")),
                         tags$td(width = "20%", numericInput("num", NULL, value = 1500))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Sensibility of the Money Demand to the Product")),
                         tags$td(width = "20%", numericInput("num", NULL, value = 0.6))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Sensibility of the Money Demand to the Interest Rate")),
                         tags$td(width = "20%", numericInput("num", NULL, value = 1))),
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
                         tags$td(width = "20%", numericInput("num", NULL, value = 55))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Autonomous Investment")),
                         tags$td(width = "20%", numericInput("num", NULL, value = 75))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Government Spending")),
                         tags$td(width = "20%", numericInput("num", NULL, value = 200))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Tax on Income")),
                         tags$td(width = "20%", numericInput("num", NULL, value = 110))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Money Supply")),
                         tags$td(width = "20%", numericInput("num", NULL, value = 200))),
                 tags$tr(width = "100%",
                         tags$td(width = "80%", div(style = "font-size:13px;", "Price Level (fixed sdsdfin the short-run)")),
                         tags$td(width = "20%", numericInput("num", NULL, value = 1))),
                 br()
               )
        )
      )
    ),
    
    mainPanel('huahahaha',
      plotOutput("islmPlot")
    )
  )
  
  
  
)


server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

shinyApp(ui, server)
