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
      textAreaInput(inputId = "myinputtext",
                  label = "Write something here:"
                  )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Display selected number of bins.
      verbatimTextOutput("outputtext"),
      g <- make_empty_graph()
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$outputtext <- renderText({
    paste0("Output: ", input$myinputtext)
  })
  
}

# Run the app.
shinyApp(ui = ui, server = server)