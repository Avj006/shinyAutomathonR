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