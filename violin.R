library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Violin or Box Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable1", label = "Select variable for X-axis", 
                  choices = colnames(iris)[-5], selected = colnames(iris)[1]),
      selectInput(inputId = "variable2", label = "Select variable for Y-axis", 
                  choices = colnames(iris)[-5], selected = colnames(iris)[2]),
      selectInput(inputId = "plot_type", label = "Select plot type", 
                  choices = c("Violin Plot", "Box Plot"), selected = "Violin Plot"),
      checkboxGroupInput(inputId = "species", label = "Select Species",
                         choices = unique(iris$Species), selected = unique(iris$Species)),
      uiOutput("color_ui")
    ),
    mainPanel(
      h4("Hey Poonam Suthar(B20ES007)"),
      plotlyOutput(outputId = "plot", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  species_colors <- function() {
    colors <- c(red = "red", green = "green", blue = "blue", yellow = "yellow", purple = "purple")
    lapply(input$species, function(s) {
      inputId <- paste0(s, "_color")
      color <- input[[inputId]]
      colors[color]
    })
  }
  
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
  
  output$plot <- renderPlotly({
    # filter data based on selected species
    filtered_data <- iris %>% 
      filter(Species %in% input$species)
    
    # create a ggplot object based on selected plot type
    if (input$plot_type == "Violin Plot") {
      p <- ggplot(filtered_data, aes_string(x = input$variable1, y = input$variable2, fill = "Species")) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE) +
        scale_fill_manual(values = species_colors()) +
        labs(x = input$variable1, y = input$variable2, title = "Violin Plot")
    } else {
      p <- ggplot(filtered_data, aes_string(x = "Species", y = input$variable1)) +
        geom_boxplot(fill = "gray", color = "black", alpha = 0.5, outlier.color = NA) +
        scale_fill_manual(values = species_colors()) +
        labs(x = "Species", y = input$variable1, title = "Box Plot")
    }
    
    # add pan and zoom functionality to each diagram
    ggplotly(p) %>% 
      layout(dragmode = "pan", hovermode = "closest") %>%     
      config(displayModeBar = TRUE) %>% 
      # reshape diagram
      subplot(nrows = 1, margin = 0.05)
  })
}

# Run the application

shinyApp(ui = ui, server = server)