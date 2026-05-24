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
      
      # NEW: Determinista o no determinista
      textOutput("automataType"),
      
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
    
    # result <- c()
    
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
        
        variable <- parts[1]
        right_side <- parts[2]
        
        terminal <- substring(right_side, 1, 1)
        variable_rs <- substring(right_side, 2, 2)
        
        if (grepl("^[a-z][A-Z]$", right_side)) {
          
          edges <- rbind(edges, data.frame(
            from = variable,
            to = variable_rs,
            label = terminal,
            stringsAsFactors = FALSE
          ))
          
        } else if (grepl("^[a-z]$", right_side)) {
          
          edges <- rbind(edges, data.frame(
            from = variable,
            to = "Z",
            label = terminal,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Mantener lógica original del output
    paste0(
      "Output:\n",
      paste(capture.output(print(edges)), collapse = "\n")
    )
  })
  
  # =========================
  # NEW: DFA / NFA CHECK
  # =========================
  output$automataType <- renderText({
    
    # reconstruimos exactamente el mismo edges SIN duplicar lógica
    lines <- strsplit(input$myinputtext, split = "\n")[[1]]
    
    edges <- data.frame(from = character(),
                        to = character(),
                        label = character(),
                        stringsAsFactors = FALSE)
    
    for (line in lines) {
      
      if(trimws(line) == ""){
        next
      }
      
      parts <- strsplit(line, "\\s*->\\s*")[[1]]
      
      if(length(parts) >= 2){
        
        variable <- parts[1]
        right_side <- parts[2]
        
        terminal <- substring(right_side, 1, 1)
        variable_rs <- substring(right_side, 2, 2)
        
        if (grepl("^[a-z][A-Z]$", right_side)) {
          
          edges <- rbind(edges, data.frame(
            from = variable,
            to = variable_rs,
            label = terminal
          ))
          
        } else if (grepl("^[a-z]$", right_side)) {
          
          edges <- rbind(edges, data.frame(
            from = variable,
            to = "Z",
            label = terminal
          ))
        }
      }
    }
    
    # DFA / NFA 
    
    seen <- c()
    
    for(i in 1:nrow(edges)) {
      
      key <- paste(edges$from[i], edges$label[i])
      
      # si ya existe combinación estado-símbolo es NFA
      if(key %in% seen){
        return("Non-deterministic")
      }
      
      seen <- c(seen, key)
    }
    
    # si no hay repeticiones si es DFA
    return("Deterministic")
  })
  
  # Grafo - ACTUALIZADO CON CURVAS REACTIVAS
  output$graphplot <- renderPlot({
    
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
    
    if(nrow(edges) == 0){
      plot.new()
      text(0.5, 0.5, "Enter valid grammar rules to see the automaton", cex = 1.2)
      return()
    }
    
    nodes <- unique(c(edges$from, edges$to))
    
    g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
    
    node_colors <- rep("lightgray", length(V(g)))
    names(node_colors) <- V(g)$name
    
    if ("S" %in% names(node_colors)) node_colors["S"] <- "lightgreen"
    if ("Z" %in% names(node_colors)) node_colors["Z"] <- "salmon"
    
    #CURVAS 
    curves <- curve_multiple(g)
    curves <- curves * 0.3
    
    set.seed(123)
    
    # DIBUJA LA GRAFICA
    plot(g, 
         edge.label = E(g)$label,
         vertex.color = node_colors,
         vertex.size = 35,
         vertex.label.color = "black",
         vertex.label.cex = 1.2,
         edge.arrow.size = 0.6,
         edge.curved = curves,  
         main = "Diagrama del Autómata")
  })
}

# Run the app.
shinyApp(ui = ui, server = server)