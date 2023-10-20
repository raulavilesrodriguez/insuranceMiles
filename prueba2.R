library(shiny)
library(DT)

# Define UI for application
ui <- fluidPage(
  titlePanel("CRUD App"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Name"),
      numericInput("age", "Age", value = 18),
      actionButton("create", "Create"),
      br(),
      textInput("search_name", "Search by name"),
      actionButton("search", "Search"),
      br(),
      textInput("update_name", "Update by name"),
      numericInput("update_age", "Age", value = 18),
      actionButton("update", "Update"),
      br(),
      textInput("delete_name", "Delete by name"),
      actionButton("delete", "Delete")
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Create reactive data frame
  data <- reactiveValues(df = data.frame(name = character(), age = numeric()))
  
  # Insert data into data frame
  observeEvent(input$create, {
    if (input$name != "") {
      data$df <- rbind(data$df, data.frame(name = input$name, age = input$age))
    }
  })
  
  # Search data from data frame
  search_result <- reactive({
    if (input$search_name != "") {
      subset(data$df, grepl(input$search_name, name))
    } else {
      data$df
    }
  })
  
  # Update data in data frame
  observeEvent(input$update, {
    if (input$update_name != "") {
      data$df[data$df$name == input$update_name, "age"] <- input$update_age
    }
  })
  
  # Delete data from data frame
  observeEvent(input$delete, {
    if (input$delete_name != "") {
      data$df <- subset(data$df, name != input$delete_name)
    }
  })
  
  # Render table output
  output$table <- renderTable({
    search_result()
  })
  veamos <- reactive({input$responses_table_rows_selected})
}

# Run the application
shinyApp(ui = ui, server = server)
