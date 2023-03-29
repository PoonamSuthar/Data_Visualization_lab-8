library(shiny)
library(ggplot2)
library(plotly)

data(iris)

ui <- fluidPage(
  titlePanel("Iris Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "xvar", label = "X-axis variable:", choices = names(iris)[1:4]),
      selectInput(inputId = "yvar", label = "Y-axis variable:", choices = names(iris)[1:4]),
      selectInput(inputId = "species", label = "Species:", choices = c("All", unique(iris$Species))),
      numericInput(inputId = "pointsize", label = "Point size:", value = 5),
      selectInput(inputId = "pointshape", label = "Point shape:", choices = c("Circle", "Square", "Diamond", "Triangle Up", "Triangle Down")),
      plotlyOutput(outputId = "scatterplot")
    ),
    mainPanel(
      plotlyOutput(outputId = "scatterplot_zoom")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$species == "All") {
      iris
    } else {
      iris[iris$Species == input$species,]
    }
  })
  
  output$scatterplot <- renderPlotly({
    ggplot(filtered_data(), aes_string(x = input$xvar, y = input$yvar, color = "Species", size = "Species", shape = "Species")) +
      geom_point() +
      scale_color_manual(values = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")) +
      scale_size_manual(values = c("setosa" = input$pointsize, "versicolor" = input$pointsize, "virginica" = input$pointsize)) +
      scale_shape_manual(values = c("setosa" = 1, "versicolor" = 2, "virginica" = 3)) +
      labs(x = input$xvar, y = input$yvar) +
      theme_bw()
  })
  
  output$scatterplot_zoom <- renderPlotly({
    ggplot(filtered_data(), aes_string(x = input$xvar, y = input$yvar, color = "Species", size = "Species", shape = "Species")) +
      geom_point() +
      scale_color_manual(values = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")) +
      scale_size_manual(values = c("setosa" = input$pointsize, "versicolor" = input$pointsize, "virginica" = input$pointsize)) +
      scale_shape_manual(values = c("setosa" = 1, "versicolor" = 2, "virginica" = 3)) +
      labs(x = input$xvar, y = input$yvar) +
      layout(
        autosize = TRUE,
        height = 500,
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
        xaxis = list(
          autorange = TRUE,
          showgrid = TRUE,
          zeroline = FALSE,
          showline = TRUE,
          ticks = "",
          showticklabels = TRUE
        ),
        yaxis = list(
          autorange = TRUE,
          showgrid = TRUE,
          zeroline = FALSE,
          showline = TRUE,
          ticks = "",
          showticklabels = TRUE
        ),
        dragmode = "pan",
        hovermode = "closest",
        hoverdistance = -1,
        spikedistance = -1,
        font = list(size = 12),
        legend = list(
          font = list(size = 12),
          title = list(text = "Species", font = list(size = 12))
        ),
        title = list(text = "Iris Dataset", font = list(size = 18))
      ) %>%
      layout(
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        boxmode = "group",
        annotations = list(
          x = 1.05,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          text = "Species",
          showarrow = FALSE,
          font = list(size = 12)
        )
      )
  })
}

shinyApp(ui, server)
            