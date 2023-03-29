
iris<- read_excel("C:/Users/Radhe-Radhe/Downloads/iris_data.xlsx")

ui <- fluidPage(
  titlePanel("Poonam Suthar_B20ES007"),
  h1("Histograms of all the properties"),
  p("Different slider bar for different histograms "),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(inputId = "bins1",
                  label = "Number of bins for Sepal Length:",
                  min = 5,
                  max = 50,
                  value = 30),
      sliderInput(inputId = "bins2",
                  label = "Number of bins for Petal Length:",
                  min = 5,
                  max = 50,
                  value = 30),
      sliderInput(inputId = "bins3",
                  label = "Number of bins for Sepal Width:",
                  min = 5,
                  max = 50,
                  value = 30),
      sliderInput(inputId = "bins4",
                  label = "Number of bins for Petal Width:",
                  min = 5,
                  max = 50,
                  value = 30),
      selectInput(inputId = "color1", 
                  label = "Histogram Color", 
                  choices = c("Blue" = "#007bc2", "Orange" = "#ff7f0e", "Green" = "#2ca02c", "Red" = "#d62728"), 
                  selected = "#007bc2"),
      
      selectInput(inputId = "color2", 
                  label = "Histogram Color", 
                  choices = c("Blue" = "#007bc2", "Orange" = "#ff7f0e", "Green" = "#2ca02c", "Red" = "#d62728"), 
                  selected = "#007bc2"),
      
      
      selectInput(inputId = "color3", 
                  label = "Histogram Color", 
                  choices = c("Blue" = "#007bc2", "Orange" = "#ff7f0e", "Green" = "#2ca02c", "Red" = "#d62728"), 
                  selected = "#007bc2"),
      
      selectInput(inputId = "color4", 
                  label = "Histogram Color", 
                  choices = c("Blue" = "#007bc2", "Orange" = "#ff7f0e", "Green" = "#2ca02c", "Red" = "#d62728"), 
                  selected = "#007bc2")
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
server <- function(input, output){
  output$hist1 <- renderPlot({
    x <- iris$SepalLengthCm
    bins <- seq(min(x), max(x), length.out = input$bins1 + 1)
    hist(x, breaks = bins, col = input$color1, border = "white",
         xlab = "Sepal Length (cm)",
         main = "Histogram of Sepal Length")
  })
  
  output$hist2 <- renderPlot({
    x <- iris$PetalLengthCm
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
    hist(x, breaks = bins, col = input$color2, border = "white",
         xlab = "Petal Length (cm)",
         main = "Histogram of Petal Length")
  })
  
  output$hist3 <- renderPlot({
    x <- iris$SepalWidthCm
    bins <- seq(min(x), max(x), length.out = input$bins3 + 1)
    hist(x, breaks = bins, col = input$color3, border = "white",
         xlab = "Sepal Width (cm)",
         main="Histogram od Sepal Width")
  })
  output$hist4 <- renderPlot({
    x <- iris$PetalWidthCm
    bins <- seq(min(x), max(x), length.out = input$bins4 + 1)
    hist(x, breaks = bins, col = input$color4, border = "white",
         xlab = "Petal Length (cm)",
         main = "Histogram of Petal Length")
  })
}

shinyApp(ui = ui, server = server)
