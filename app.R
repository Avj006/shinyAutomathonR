library(shiny)
library(igraph)

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap"),
    tags$style(HTML("
    
      /* ---- RESET & BASE ---- */
      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        font-family: 'IBM Plex Sans', sans-serif;
        background-color: #f0f2f5;
        color: #1c2b3a;
      }

      .container-fluid { padding: 0 !important; }

      /* ---- HEADER ---- */
      .app-header {
        background-color: #1c2b3a;
        padding: 28px 40px;
        border-bottom: 3px solid #2e86c1;
      }
      .app-header h1 {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 20px;
        font-weight: 600;
        color: #ffffff;
        letter-spacing: -0.3px;
        margin-bottom: 4px;
      }
      .app-header p {
        font-size: 13px;
        color: #8fa3b4;
        font-weight: 300;
        letter-spacing: 0.4px;
      }

      /* ---- LAYOUT ---- */
      .app-body {
        display: flex;
        min-height: calc(100vh - 88px);
      }

      /* ---- SIDEBAR ---- */
      .app-sidebar {
        width: 340px;
        min-width: 340px;
        background-color: #ffffff;
        border-right: 1px solid #dde3ea;
        padding: 28px 24px;
        display: flex;
        flex-direction: column;
        gap: 24px;
      }

      .sidebar-section-label {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 10px;
        font-weight: 600;
        letter-spacing: 1.5px;
        text-transform: uppercase;
        color: #8fa3b4;
        margin-bottom: 10px;
      }

      /* ---- TEXTAREA ---- */
      .form-group { margin-bottom: 0 !important; }

      .form-group label {
        font-size: 12px;
        font-weight: 500;
        color: #4a6075;
        margin-bottom: 6px;
        display: block;
      }

      #myinputtext {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 13px;
        background-color: #f7f9fb;
        border: 1.5px solid #dde3ea;
        border-radius: 6px;
        color: #1c2b3a;
        padding: 12px;
        resize: vertical;
        transition: border-color 0.15s;
        width: 100% !important;
        line-height: 1.7;
      }
      #myinputtext:focus {
        border-color: #2e86c1;
        outline: none;
        background-color: #fff;
      }

      /* ---- EXAMPLE BUTTONS ---- */
      .btn-row {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 8px;
      }
      .btn-row .action-button {
        font-family: 'IBM Plex Sans', sans-serif;
        font-size: 12px;
        font-weight: 500;
        border: 1.5px solid #dde3ea;
        background-color: #f7f9fb;
        color: #4a6075;
        border-radius: 5px;
        padding: 8px 10px;
        cursor: pointer;
        transition: all 0.15s;
        text-align: center;
        width: 100%;
      }
      .btn-row .action-button:hover {
        background-color: #e8f0f7;
        border-color: #2e86c1;
        color: #2e86c1;
      }
      .btn-clear-wrap .action-button {
        font-family: 'IBM Plex Sans', sans-serif;
        font-size: 12px;
        font-weight: 500;
        border: 1.5px solid #f5c6cb;
        background-color: #fff5f5;
        color: #c0392b;
        border-radius: 5px;
        padding: 8px 14px;
        cursor: pointer;
        transition: all 0.15s;
        width: 100%;
        margin-top: 8px;
      }
      .btn-clear-wrap .action-button:hover {
        background-color: #fdecea;
        border-color: #c0392b;
      }

      /* ---- LEGEND ---- */
      .legend-list {
        list-style: none;
        display: flex;
        flex-direction: column;
        gap: 7px;
      }
      .legend-list li {
        display: flex;
        align-items: center;
        gap: 10px;
        font-size: 12.5px;
        color: #4a6075;
      }
      .legend-dot {
        width: 14px;
        height: 14px;
        border-radius: 50%;
        flex-shrink: 0;
        border: 1.5px solid rgba(0,0,0,0.12);
      }

      /* ---- MAIN PANEL ---- */
      .app-main {
        flex: 1;
        padding: 28px 32px;
        display: flex;
        flex-direction: column;
        gap: 20px;
      }

      /* ---- CARDS ---- */
      .card {
        background-color: #ffffff;
        border: 1px solid #dde3ea;
        border-radius: 8px;
        overflow: hidden;
      }
      .card-header {
        padding: 13px 20px;
        border-bottom: 1px solid #eef1f4;
        background-color: #f7f9fb;
      }
      .card-header span {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 11px;
        font-weight: 600;
        letter-spacing: 1px;
        text-transform: uppercase;
        color: #8fa3b4;
      }
      .card-body {
        padding: 20px;
      }

      /* ---- STATS ROW ---- */
      .stats-row {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 16px;
      }
      .stat-block {
        border-left: 3px solid #2e86c1;
        padding-left: 14px;
      }
      .stat-block .stat-label {
        font-size: 11px;
        font-weight: 600;
        letter-spacing: 0.8px;
        text-transform: uppercase;
        color: #8fa3b4;
        margin-bottom: 4px;
      }

      /* ---- AUTOMATA TYPE TEXT OUTPUT ---- */
      #automataType {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 18px;
        font-weight: 600;
      }
      /* Coloring applied via server-side but we provide fallback styles */
      .deterministic   { color: #27ae60; }
      .nondeterministic { color: #e74c3c; }

      /* ---- STATS UI OUTPUT ---- */
      #stats p {
        font-size: 13px;
        color: #4a6075;
        margin-bottom: 4px;
        line-height: 1.5;
      }
      #stats strong {
        color: #1c2b3a;
        font-weight: 600;
      }

      /* ---- GRAPH AREA ---- */
      .shiny-plot-output {
        width: 100% !important;
      }

      /* ---- DIVIDER ---- */
      .section-divider {
        height: 1px;
        background-color: #eef1f4;
        margin: 0;
      }

      /* ---- SHINY OVERRIDES ---- */
      .row { margin: 0 !important; }
      .col-sm-4, .col-sm-8 { padding: 0 !important; }
    "))
  ),
  
  # ---- HEADER ----
  div(class = "app-header",
      h1("Generador de Automatas desde Gramatica Regular"),
      p("Convierte gramaticas regulares a automatas finitos de forma reactiva")
  ),
  
  div(class = "app-body",
      
      # ===================== SIDEBAR =====================
      div(class = "app-sidebar",
          
          # --- Input ---
          div(
            div(class = "sidebar-section-label", "Entrada"),
            textAreaInput(
              inputId  = "myinputtext",
              label    = "Reglas de produccion",
              value    = "S -> aA\nS -> bA\nA -> aB\nA -> bB\nA -> a\nB -> aA\nB -> bA",
              rows     = 11,
              width    = "100%"
            )
          ),
          
          # --- Examples ---
          div(
            div(class = "sidebar-section-label", "Ejemplos"),
            div(class = "btn-row",
                actionButton("btn_dfa",     "DFA Simple"),
                actionButton("btn_nfa",     "NFA"),
                actionButton("btn_complex", "Complejo")
            ),
            div(class = "btn-clear-wrap",
                actionButton("btn_clear", "Limpiar entrada")
            )
          ),
          
          # --- Legend ---
          div(
            div(class = "sidebar-section-label", "Leyenda"),
            tags$ul(class = "legend-list",
                    tags$li(
                      div(class = "legend-dot",
                          style = "background-color:#90EE90; border-color: #5dbb6a;"),
                      "Estado inicial (S)"
                    ),
                    tags$li(
                      div(class = "legend-dot",
                          style = "background-color:#FA8072; border-color: #e05a4a;"),
                      "Estado final (Z)"
                    ),
                    tags$li(
                      div(class = "legend-dot",
                          style = "background-color:#D3D3D3; border-color: #aaa;"),
                      "Estados intermedios"
                    )
            )
          )
      ),
      
      # ===================== MAIN PANEL =====================
      div(class = "app-main",
          
          # -- Top row: Type + Stats --
          div(class = "card",
              div(class = "card-body",
                  div(class = "stats-row",
                      
                      div(class = "stat-block",
                          div(class = "stat-label", "Tipo de automata"),
                          textOutput("automataType")
                      )
                  )
              )
          ),
          
          # -- Graph --
          div(class = "card", style = "flex: 1;",
              div(class = "card-header",
                  tags$span("Diagrama del automata")
              ),
              div(class = "card-body",
                  plotOutput("graphplot", height = "520px")
              )
          ),
          
          # outputtext se mantiene oculto (logica intacta, sin mostrarse en UI)
          tags$div(style = "display:none;",
                   verbatimTextOutput("outputtext")
          )
      )
  )
)

# ============================================================
# SERVER — SIN NINGUN CAMBIO
# ============================================================
server <- function(input, output, session) {
  
  observeEvent(input$btn_dfa, {
    updateTextAreaInput(session, "myinputtext", 
                        value = "S -> aA\nA -> bB\nB -> c")
  })
  
  observeEvent(input$btn_nfa, {
    updateTextAreaInput(session, "myinputtext",
                        value = "S -> aA\nS -> aB\nA -> b\nB -> c")
  })
  
  observeEvent(input$btn_complex, {
    updateTextAreaInput(session, "myinputtext",
                        value = "S -> aA\nS -> bA\nA -> aB\nA -> bB\nA -> a\nB -> aA\nB -> bA")
  })
  
  observeEvent(input$btn_clear, {
    updateTextAreaInput(session, "myinputtext", value = "")
  })
  
  output$outputtext <- renderText({
    lines <- strsplit(input$myinputtext, split = "\n")[[1]]
    edges <- data.frame(from = character(), to = character(), 
                        label = character(), stringsAsFactors = FALSE)
    for (line in lines) {
      if(trimws(line) == "") next
      parts <- strsplit(line, "\\s*->\\s*")[[1]]
      if(length(parts) >= 2){
        variable  <- parts[1]
        right_side <- parts[2]
        terminal  <- substring(right_side, 1, 1)
        variable_rs <- substring(right_side, 2, 2)
        if (grepl("^[a-z][A-Z]$", right_side)) {
          edges <- rbind(edges, data.frame(from = variable, to = variable_rs,
                                           label = terminal, stringsAsFactors = FALSE))
        } else if (grepl("^[a-z]$", right_side)) {
          edges <- rbind(edges, data.frame(from = variable, to = "Z",
                                           label = terminal, stringsAsFactors = FALSE))
        }
      }
    }
    paste0("Output:\n", paste(capture.output(print(edges)), collapse = "\n"))
  })
  
  output$automataType <- renderText({
    lines <- strsplit(input$myinputtext, split = "\n")[[1]]
    edges <- data.frame(from = character(), to = character(),
                        label = character(), stringsAsFactors = FALSE)
    for (line in lines) {
      if(trimws(line) == "") next
      parts <- strsplit(line, "\\s*->\\s*")[[1]]
      if(length(parts) >= 2){
        variable   <- parts[1]
        right_side <- parts[2]
        terminal   <- substring(right_side, 1, 1)
        variable_rs <- substring(right_side, 2, 2)
        if (grepl("^[a-z][A-Z]$", right_side)) {
          edges <- rbind(edges, data.frame(from = variable, to = variable_rs, label = terminal))
        } else if (grepl("^[a-z]$", right_side)) {
          edges <- rbind(edges, data.frame(from = variable, to = "Z", label = terminal))
        }
      }
    }
    seen <- c()
    for(i in 1:nrow(edges)) {
      key <- paste(edges$from[i], edges$label[i])
      if(key %in% seen) return("Non-deterministic")
      seen <- c(seen, key)
    }
    return("Deterministic")
  })
  
  
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
        if (grepl("^[a-z][A-Z]$", right_side)) {
          symbol   <- substr(right_side, 1, 1)
          to_state <- substr(right_side, 2, nchar(right_side))
          edges <- rbind(edges, data.frame(from = from_state, to = to_state, 
                                           label = symbol, stringsAsFactors = FALSE))
        } else if (grepl("^[a-z]$", right_side)) {
          symbol   <- right_side
          to_state <- "Z"
          edges <- rbind(edges, data.frame(from = from_state, to = to_state, 
                                           label = symbol, stringsAsFactors = FALSE))
        } else if (right_side == "ε") {
          symbol   <- "ε"
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
    curves <- curve_multiple(g)
    curves <- curves * 0.8
    set.seed(123)
    plot(g, 
         edge.label      = E(g)$label,
         vertex.color    = node_colors,
         vertex.size     = 35,
         vertex.label.color = "black",
         vertex.label.cex   = 1.2,
         edge.arrow.size    = 0.6,
         edge.curved        = curves,
         main = "Diagrama del Automata")
  })
}

shinyApp(ui = ui, server = server)