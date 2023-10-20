library(shiny)
library(DT)

ui <- fluidPage(
  DTOutput("responses_table"),
  # Add other UI elements as needed
  verbatimTextOutput("code")
)

server <- function(input, output) {
  # Create a sample data frame (replace with your own data)
  data <- data.frame(
    Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
    Score = c(90, 85, 78, 92, 88)
  )
  
  # Render the DataTable
  output$responses_table <- renderDT({
    datatable(data, selection = 'multiple')
  })
  
  # Observe the selected rows
  observe({
    selected_rows <- input$responses_table_rows_selected
    # You can now use the selected_rows variable as needed in your application
    # For example, to filter the data based on selected rows:
    selected_data <- data[selected_rows, ]
    # Or perform any other desired actions
    output$code <- renderPrint({selected_data})
  })
}

shinyApp(ui, server)
