library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

ui <- fluidPage(
  titlePanel("Scatter Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "xvar", label = "X-axis variable", 
                  choices = colnames(iris)[-5], selected = colnames(iris)[1]),
      selectInput(inputId = "yvar", label = "Y-axis variable", 
                  choices = colnames(iris)[-5], selected = colnames(iris)[2]),
      checkboxGroupInput(inputId = "species", label = "Select Species",
                         choices = unique(iris$Species), selected = unique(iris$Species)),
      sliderInput(inputId = "size", label = "Select point size",
                  min = 1, max = 10, value = 5),
      selectInput(inputId = "shape", label = "Select point shape",
                  choices = c("circle", "square", "triangle", "diamond"), selected = "circle"),
      uiOutput("color_ui")
    ),
    mainPanel(
      h2("Hey Poonam Suthar(B20ES007)"),
      plotlyOutput(outputId = "scatterplot",height = "600px", width = "100%") # Set width to 100%
    )
  )
)

server <- function(input, output) {
  
  output$color_ui <- renderUI({
    species_colors <- lapply(unique(iris$Species), function(s) {
      tagList(
        tags$label(paste0("Color for ", s)),
        selectInput(inputId = paste0(s, "_color"), "", 
                    choices = c("red", "green", "blue", "yellow", "purple"), 
                    selected = "red")
      )
    })
    do.call(tagList, species_colors)
  })
  
  observe({
    filtered_data <- iris %>% 
      filter(Species %in% input$species)
    
    species_colors <- lapply(unique(filtered_data$Species), function(s) {
      if(!is.null(input[[paste0(s, "_color")]])){
        input[[paste0(s, "_color")]]
      } else {
        "black" # set to a default color if selected color is NULL
      }
    })
    names(species_colors) <- unique(filtered_data$Species)
    
    p <- ggplot(data = filtered_data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]], 
                                          color = Species, size = Species, shape = Species)) +
      geom_point() +
      
      scale_color_manual(values = species_colors) +
      scale_size_manual(values = c("setosa" = input$size, "versicolor" = input$size, "virginica" = input$size)) +
      scale_shape_manual(values = c("setosa" = input$shape, "versicolor" = input$shape, "virginica" = input$shape)) +
      labs(x = input$xvar, y = input$yvar, title = "Scatter Plot") +
      theme_bw()
    
    output$scatterplot <- renderPlotly({
      ggplotly(p) %>% 
        layout(dragmode = "pan", hovermode = "closest") %>% 
        config(displayModeBar = TRUE)
    })
  })
}

shinyApp(ui = ui, server = server)
