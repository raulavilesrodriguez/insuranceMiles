library(config)
library(DBI)
library(lubridate)
library(glue)
library(fontawesome)
library(leaflet)
library(shiny)
library(DT)
library(RPostgreSQL)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)
library(tidyverse)
library(shinydashboard)
library(shinyauthr)

#Read the database connection parameters from the config.yml
config_file <- "config.yml"
config <- config::get(file = config_file)

# Create a database connection
db <- dbPool(RPostgres::Postgres(),
                           host = config$host,
                           port = config$port,
                           dbname = config$dbname,
                           user = config$user,
                           password = config$password)

# a user who has not visited the app for this many days
# will be asked to login with user name and password again
cookie_expiry <- 7 # Days until session expires


# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  This function saves to your database.

add_sessionid_to_db <- function(users, sessionid, conn = db) {
  tibble(users = users, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE)
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}


# dataframe that holds usernames, passwords and other user data
user_base <- dbReadTable(db, "acceso") |> as_tibble()


# create the dataframe to 'titular' database. 
responses_df <- data.frame(row_id = character(),
                          nombre = character(),
                          sexo = character(),
                          edad = as.numeric(character()),
                          millas = as.numeric(character()),
                          cedula = character(),
                          email = character(),
                          comentario = character(),
                          creado = as.Date(character()),
                          stringsAsFactors = FALSE
                          )

# Create responses tabl in PostegreSQL
dbWriteTable(db, 
             "responses_df",
             responses_df,
             overwrite = FALSE,
             append = TRUE,
             )

# Check if 'row_id' is already a primary key
existing_primary_key_query  <- dbGetQuery(db, 
                                           
                    "SELECT column_name, data_type
FROM information_schema.columns
WHERE table_name = 'responses_df'
AND column_name = 'row_id'
AND column_name IN (
    SELECT column_name
    FROM information_schema.table_constraints
    JOIN information_schema.key_column_usage USING (constraint_catalog, constraint_schema, constraint_name, table_catalog, table_schema, table_name)
    WHERE constraint_type = 'PRIMARY KEY'
    AND table_name = 'responses_df'
);")


if (nrow(existing_primary_key_query) == 0) {
  # 'row_id' is not a primary key, so set it as one
  dbExecute(db, "ALTER TABLE responses_df ADD PRIMARY KEY (row_id);")
}

# Check if 'cedula' is already a unique key
existing_unique_key_query <- dbGetQuery(db,
                "SELECT column_name, data_type
FROM information_schema.columns
WHERE table_name = 'responses_df'
AND column_name = 'cedula'
AND column_name IN (
    SELECT column_name
    FROM information_schema.table_constraints
    JOIN information_schema.key_column_usage USING (constraint_catalog, constraint_schema, constraint_name, table_catalog, table_schema, table_name)
    WHERE constraint_type = 'UNIQUE'
    AND table_name = 'responses_df'
);")

if (nrow(existing_unique_key_query) == 0) {
  # 'cedula' is not a unique key, so set it as one
  dbExecute(db, "ALTER TABLE responses_df ADD CONSTRAINT unique_cedula UNIQUE (cedula);")
}


# to mark * any fields in the entry form that are mandatory
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"


#--------Shiny APP---------
ui <- dashboardPage(
  dashboardHeader(
    title = "Millas app",
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout") # add logout button UI
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        icon("github"),
        href = "https://raulaviles.netlify.app/",
        title = "Autor"
      )
    )
  ),
  dashboardSidebar(
    collapsed = TRUE,
    div(icon("circle-user"), HTML("&nbsp;"), textOutput("welcome"), style = "display: flex; align-items: center; padding: 20px"),
    sidebarMenu(
      menuItem("Dasboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$style(HTML(".content { padding: 50px; }")),
    shinyauthr::loginUI(
      id = "login", 
      cookie_expiry = cookie_expiry,
      title = "Ingreso Seguro App"
    ),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    tags$div(id = "tabs",
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  actionButton("add_button", "Add", icon("plus")),
                  actionButton("edit_button", "Edit", icon("edit")),
                  actionButton("delete_button", "Delete", icon("trash-alt")),
                ),
                br(),
                fluidRow(width="100%",
                         dataTableOutput("responses_table", width = "100%")
                )
        ),
        tabItem(tabName = "widgets"
                
                
        )
      )
    )
    
  )
)

server <- function(input, output, session) {
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = users,
    pwd_col = password,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessionids_from_db,
    cookie_setter = add_sessionid_to_db,
    log_out = reactive(logout_init())
  )
  
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::show("tabs")
      shinyjs::show(selector = ".main-header")
      
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::hide("tabs")
      shinyjs::hide(selector = ".main-header")
    }
  })
  
  user_info <- reactive({
    credentials()$info
  })
  
  output$welcome <- renderText({
    req(credentials()$user_auth)
    
    glue("Bienvenido {user_info()$name}")
  })
  
  # Enter the inputs to make the df reactive to any input changes.
  responses_df <- reactive({
    
    input$submit
    input$submit_edit
    input$delete_button
    
    dbReadTable(db, "responses_df")
  })
  
  # Enter the name of the fields that should be manditory to fill out
  fieldsMandatory <- c("nombre", "sexo", "edad", "millas", "cedula", "email")
  
  # Function to observe if all mandatory fields are filled out. 
  #If TRUE the submit button will become activated
  observe({
    
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", 
                         condition = mandatoryFilled)
  })
  
  #______Entry form__________
  entry_form <- function(button_id){
    
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c("250px", "100px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("nombre", labelMandatory("Nombre"), placeholder = ""),
                  selectInput("sexo", labelMandatory("Sexo"), multiple = FALSE, choices = c("", "M", "F"))
                ),
                sliderInput("edad", labelMandatory("Edad"), 0, 100, 1, ticks = TRUE, width = "354px"),
                textInput("millas", labelMandatory("Millas"), placeholder = ""),
                textInput("cedula", labelMandatory("Cedula"), placeholder = ""),
                textInput("email", labelMandatory("Email"), placeholder = ""),
                textAreaInput("comentario", "Comentario", placeholder = "", height = 100, width = "354px"),
                helpText(labelMandatory(""), paste("Campo Obligatorio")),
                actionButton(button_id, "Submit")
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  #____Add Data_____
  # Function to save the data into df format
  formData <- reactive({
    
    formData <- data.frame(row_id = UUIDgenerate(),
                           nombre = input$nombre,
                           sexo = input$sexo,
                           edad = input$edad,
                           millas = input$millas,
                           cedula = input$cedula,
                           email = input$email,
                           comentario = input$comentario,
                           creado = as.character(format(Sys.Date(), format="%Y-%m-%d")),
                           stringsAsFactors = FALSE)
    return(formData)
  })
  
  # Function to append data to the SQL table
  appendData <- function(data){
    quary <- dbWriteTable(db, "responses_df", data, append = TRUE)
  }
  
  # When add button is clicked it will activate the entry_form with an 
  #action button called submit
  observeEvent(input$add_button, priority = 20,{
    
    entry_form("submit")
    
  })
  
  # reset and the modal is removed
  observeEvent(input$submit, priority = 20,{
    
    appendData(formData())
    shinyjs::reset("entry_form")
    removeModal()
    
  })
  
  
  output$responses_table <- DT::renderDataTable({
    table <- responses_df() %>% select(-row_id) 
    names(table) <- c(
      "Nombre", "Sexo", "Edad", "Millas", "Cédula", "Email", "Commentario", "Creado")
    table <- datatable(table, 
                       rownames = FALSE,
                       options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
}

shinyApp(ui = ui, server = server)









