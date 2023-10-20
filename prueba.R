# Load required libraries
library(shiny)
library(rhandsontable)

# Sample data
data <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  Age = c(25, 30, 35, 40, 45)
)

# Define the UI
ui <- fluidPage(
  titlePanel("Editable Data Table Example"),
  rHandsontableOutput("editTable"),
  verbatimTextOutput("editedData")
)

# Define the server logic
server <- function(input, output, session) {
  # Create an editable data table
  output$editTable <- renderRHandsontable({
    rhandsontable(data)
  })
  
  # Show the edited data
  output$editedData <- renderPrint({
    edited_data <- hot_to_r(input$editTable)
    edited_data
  })
}

# Run the Shiny app
shinyApp(ui, server)
