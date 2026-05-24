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

    edges <- data.frame(from = character(), to = character(), 
                        label = character(), stringsAsFactors = FALSE)

    for (line in lines) {

      # Ignorar líneas vacías
      if(trimws(line) == ""){
        next
      }

      # Separar producción
      parts <- strsplit(line, "\\s*->\\s*")[[1]]

      # Validar producción correcta
      if(length(parts) >= 2){

        left_side <- parts[1]
        right_side <- parts[2]

        if (grepl("^[a-z][A-Z]$", right_side)) {
          
        }

        # Guardar resultado
        result <- c(
          result,
          paste("Izq:", left_side, "Der:", right_side)
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

  # Regex para el lado derecho:
  # ^[a-z][A-Z]$
  # ^[a-z]$
  # ^ε$

  # Texto estático del automata: HASRDCODEADO por el momento
  output$automataType <- renderText({
    "Deterministic"
  })

  # Grafo - ACTUALIZADO CON CURVAS REACTIVAS
  output$graphplot <- renderPlot({
    
    # Build edges from input (same logic as your original)
    lines <- strsplit(input$myinputtext, split = "\n")[[1]]
    
    edges <- data.frame(from = character(), to = character(), 
                        label = character(), stringsAsFactors = FALSE)
    
    for (line in lines) {
      if(trimws(line) == "") next
      
      parts <- strsplit(line, "\\s*->\\s*")[[1]]
      
      if(length(parts) >= 2){
        from_state <- parts[1]
        right_side <- parts[2]
        
        # A -> aB
        if (grepl("^[a-z][A-Z]$", right_side)) {
          symbol <- substr(right_side, 1, 1)
          to_state <- substr(right_side, 2, nchar(right_side))
          edges <- rbind(edges, data.frame(from = from_state, to = to_state, 
                                           label = symbol, stringsAsFactors = FALSE))
        }
        # A -> a
        else if (grepl("^[a-z]$", right_side)) {
          symbol <- right_side
          to_state <- "Z"
          edges <- rbind(edges, data.frame(from = from_state, to = to_state, 
                                           label = symbol, stringsAsFactors = FALSE))
        }
        # A -> ε
        else if (right_side == "ε") {
          symbol <- "ε"
          to_state <- "Z"
          edges <- rbind(edges, data.frame(from = from_state, to = to_state, 
                                           label = symbol, stringsAsFactors = FALSE))
        }
      }
    }
    
    # If no edges, show placeholder
    if(nrow(edges) == 0){
      plot.new()
      text(0.5, 0.5, "Enter valid grammar rules to see the automaton", cex = 1.2)
      return()
    }
    
    # Get all unique nodes
    nodes <- unique(c(edges$from, edges$to))
    
    # Create graph
    g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
    
    # Assign colors to nodes
    node_colors <- rep("lightgray", length(V(g)))
    names(node_colors) <- V(g)$name
    
    if ("S" %in% names(node_colors)) node_colors["S"] <- "lightgreen"
    if ("Z" %in% names(node_colors)) node_colors["Z"] <- "salmon"
    
    # Calculate curves for multiple edges (THIS MAKES ARROWS CURVED)
    curves <- curve_multiple(g)
    curves <- curves * 0.5  # Adjust this value (0.3 = more curve, 0.7 = less curve)
    
    # Set seed for consistent layout
    set.seed(123)
    
    # Draw graph with curved edges
    plot(g, 
         edge.label = E(g)$label,
         vertex.color = node_colors,
         vertex.size = 35,
         vertex.label.color = "black",
         vertex.label.cex = 1.2,
         edge.arrow.size = 0.6,
         edge.curved = curves,  # <-- THIS IS THE KEY ADDITION
         main = "Diagrama del Autómata")
  })
}

# Run the app.
shinyApp(ui = ui, server = server)