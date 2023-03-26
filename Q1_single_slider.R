library(shiny)
iris<- read_excel("C:/Users/Radhe-Radhe/Downloads/iris_data.xlsx")
ui <- fluidPage(
  titlePanel("Hello World"),
  h1("Histograms of all the properties"),
  p("Same slider bar for all plots "), 

  sidebarLayout(
    sidebarPanel(
      
      sliderInput(inputId = "bins",
                  label = "Number of bins for Sepal Length:",
                  min = 5,
                  max = 50,
                  value = 30),
  
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 6,
          plotOutput(outputId = "hist1", height = "400px", width = "100%")
        ),
        column(
          width = 6,
          plotOutput(outputId = "hist2", height = "400px", width = "100%")
        )
      ),
      fluidRow(
        column(
          width = 6,
          plotOutput(outputId = "hist3", height = "400px", width = "100%")
        ),
        column(
          width = 6,
          plotOutput(outputId = "hist4", height = "400px", width = "100%")
        )
      )
    )
  )
)

# Define server logic required to draw histograms ----
server <- function(input, output) {
  output$hist1 <- renderPlot({
    x <- iris$SepalLengthCm
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Sepal Length (cm)",
         main = "Histogram of Sepal Length")
  })
  
  output$hist2 <- renderPlot({
    x <- iris$PetalLengthCm
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#ff7f0e", border = "white",
         xlab = "Petal Length (cm)",
         main = "Histogram of Petal Length")
  })
  output$hist3 <- renderPlot({
    x <- iris$SepalWidthCm
    bins <- seq(min(x), max(x), length.out = input$bins+ 1)
    hist(x, breaks = bins, col = "#2ca02c", border = "white",
         xlab = "Sepal Length (cm)",
         main = "Histogram of Sepal Depth")
  })
  
  output$hist4 <- renderPlot({
    x <- iris$PetalWidthCm
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#d62728", border = "white",
         xlab = "Petal Length (cm)",
         main = "Histogram of Petal Depth")
  })
}

shinyApp(ui = ui, server = server)
