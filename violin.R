library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)

data(iris)

ui <- fluidPage(
  
  
  titlePanel("Poonam SUthar_B20ES007"),
  h1("Scatter Plot"),
  p("Scatter plot of the iris dataset"), 
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-label", "Select Plot Type:"),
      selectInput("plot_type", label = NULL, 
                  choices = c("Box Plot", "Violin Plot"), 
                  selected = "Violin Plot")
      
    ),
    
    mainPanel(
      div(class = "plot-title", "Sepal Length by Species"),
      plotlyOutput("iris_plot")
    )
  )
)

server <- function(input, output) {
  
  iris_data <- reactive({
    if(input$plot_type == "Box Plot") {
      iris %>%
        pivot_longer(-Species, names_to = "variable", values_to = "value") %>%
        ggplot(aes(x = Species, y = value, fill = Species)) +
        geom_boxplot() +
        theme_classic() 
    } else {
      iris %>%
        ggplot(aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_violin() +
        geom_boxplot(width = 0.1, fill = "white", alpha = 0) +
        theme_classic() 
    }
  })
  
  output$iris_plot <- renderPlotly({
    ggplotly(iris_data(), tooltip = c("Species", "value")) %>%
      layout(dragmode = "zoom",
             xaxis = list(title = "Species"),
             yaxis = list(title = "Sepal Length")) %>%
      config(displayModeBar = FALSE)
  })
  
}

shinyApp(ui, server)
