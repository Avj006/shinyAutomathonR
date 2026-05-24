library(shiny)
library(igraph)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input text area
      textAreaInput(
        inputId = "myinputtext",
        label = "Write something here:"
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Tu output original
      verbatimTextOutput("outputtext"),
      
      # Aquí se mostrará el grafo
      plotOutput("graphplot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # CONSERVADO igual
  output$outputtext <- renderText({
    paste0("Output: ", input$myinputtext)
  })
  
  # Grafo
  output$graphplot <- renderPlot({
    
    g <- graph_from_edgelist(
      matrix(c(
        "S","A",
        "S","B",
        "A","B",
        "A","Z",
        "B","Z"
      ),
      byrow = TRUE,
      ncol = 2),
      directed = TRUE
    )
    
    # Etiquetas de transiciones
    E(g)$label <- c("a", "b", "b", "c", "c")
    
    # Colores de nodos
    V(g)$color <- c("green", "white", "white", "red")
    
    # Dibujar grafo
    plot(
      g,
      edge.arrow.size = 0.5,
      vertex.size = 30,
      vertex.label.cex = 1.2
    )
    
  })
  
}

# Run the app.
shinyApp(ui = ui, server = server)