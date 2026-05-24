library(shiny) # reactivo
library(igraph) # para el automata visual

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Generador de Autómatas"),
  
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
      
      # Output de texto
      verbatimTextOutput("outputtext"),
      
      # Aquí se mostrará el grafo
      plotOutput("graphplot")
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {

  # Output de texto
  output$outputtext <- renderText({

    # Separar por líneas
    lines <- strsplit(input$myinputtext, split = "\n")[[1]]

    result <- c()

    for (line in lines) {

      # Ignorar líneas vacías
      if(trimws(line) == ""){
        next
      }

      # Separar producción
      parts <- strsplit(line, "\\s*->\\s*")[[1]]

      # Validar producción correcta
      if(length(parts) >= 2){

        left_s <- parts[1]
        right_s <- parts[2]

        # Guardar resultado
        result <- c(
          result,
          paste("Izq:", left_s, "Der:", right_s)
        )
      }
    }

    # Mantener lógica original del output
    paste0(
      "Output:\n",
      paste(result, collapse = "\n")
    )
  })

  # Tipos de producciones
  # A -> aB
  # A -> a
  # A -> \epsilon
  # Si se tienen expresiones válidas que son estas tres pues si haga el plot

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