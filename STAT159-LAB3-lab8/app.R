library(shiny)
library(ggplot2)
library(stringr)

dat <- read.csv('Advertising.csv', row.names = 1) 
sales = dat$Sales

ui <- fluidPage(
  selectInput(inputId = "variable", 
              label = "Variable:", c("TV" = "TV", "Radio" = "Radio", "Newspaper" = "Newspaper")),
  plotOutput("scatterplot")
)

server <- function(input, output) {
  output$scatterplot <- renderPlot({
    var2 = as.vector(unlist(subset(dat, select = input$variable)))
    plot(var2, sales, pch = 19, col = "#8e8f94", xlab = input$variable, ylab = "Sales")
    abline(lm(sales~var2), col = "#5679DF", lwd = 3)
    segments(var2, sales, var2, predict(lm(sales~var2)), col='red')
  })
}

shinyApp(ui = ui, server = server)
